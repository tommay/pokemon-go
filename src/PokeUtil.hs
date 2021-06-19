module PokeUtil (
  addStats,
  setLevel,
  setMoves,
  levelToString,
  evolveFully,
  evolveFullyWithCandy,
  evolveSpeciesFullyWithCandy,
  evolutionChains,
  isFirstEvolution,
) where

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified MakePokemon
import           Move (Move)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Stats
import           Stats (Stats)
import qualified Util

import qualified Data.List as List
import qualified Data.List.Extra
import qualified Text.Printf as Printf

import qualified Debug

addStats :: Epic.MonadCatch m => GameMaster -> MyPokemon -> m MyPokemon
addStats gameMaster myPokemon = do
  pokemonBase <- GameMaster.getPokemonBase gameMaster $
    MyPokemon.species myPokemon
  return $ MyPokemon.setStats myPokemon $
    makeStats gameMaster pokemonBase $ MyPokemon.ivs myPokemon

makeStats :: GameMaster -> PokemonBase -> IVs -> Stats
makeStats gameMaster pokemonBase ivs =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster $ IVs.level ivs
      makeStat getBaseStat getIvStat =
        (fromIntegral $ getBaseStat pokemonBase + getIvStat ivs) * cpMultiplier
  in Stats.new
       (makeStat PokemonBase.attack IVs.attack)
       (makeStat PokemonBase.defense IVs.defense)
       (floor (makeStat PokemonBase.stamina IVs.stamina))

setLevel :: GameMaster -> Float -> Pokemon -> Pokemon
setLevel gameMaster level pokemon =
  let ivs = IVs.setLevel (Pokemon.ivs pokemon) level
  in MakePokemon.makeForWhatever
       gameMaster
       ivs
       (Pokemon.pname pokemon)
       (Pokemon.base pokemon)
       (Pokemon.quick pokemon)
       (Pokemon.charge pokemon)

setMoves :: GameMaster -> Move -> Move -> Pokemon -> Pokemon
setMoves gameMaster quick charge pokemon =
  MakePokemon.makeForWhatever
    gameMaster
    (Pokemon.ivs pokemon)
    (Pokemon.pname pokemon)
    (Pokemon.base pokemon)
    quick
    charge

levelToString :: Float -> String
levelToString level =
  if fromIntegral (floor level) == level
    then show $ floor level
    else Printf.printf "%.1f" level

evolveFully :: Epic.MonadCatch m =>
  GameMaster -> Maybe String -> MyPokemon -> m MyPokemon
evolveFully gameMaster maybeTarget myPokemon = do
  (evolvedPokemon, _) <- evolveFullyWithCandy gameMaster maybeTarget myPokemon
  return evolvedPokemon

evolveFullyWithCandy :: Epic.MonadCatch m =>
  GameMaster -> Maybe String -> MyPokemon -> m (MyPokemon, Int)
evolveFullyWithCandy gameMaster maybeTarget myPokemon = do
  let species = MyPokemon.species myPokemon
  (evolvedSpecies, candy) <-
    evolveSpeciesFullyWithCandy gameMaster False maybeTarget species
  return $ (MyPokemon.setSpecies myPokemon evolvedSpecies, candy)

-- If there is a target, return it from all the evolutions or fail
-- if it's not an evolution.
-- Otherwise return the final evolution if there is only one chain.
--
evolveSpeciesFullyWithCandy :: Epic.MonadCatch m =>
  GameMaster -> Bool -> Maybe String -> String -> m (String, Int)
evolveSpeciesFullyWithCandy gameMaster traded maybeTarget species = do
  chains <- evolutionChains gameMaster traded (species, 0)
  evolution <- case maybeTarget of
    Just target ->
      case List.find (matchWithNormal (Util.toLower target) .
          Util.toLower . fst) (concat chains) of
        Nothing -> Epic.fail $ species ++ " does not evolve to " ++ target
        Just evolution -> return evolution
    Nothing ->
      case chains of
        [chain] -> return $ List.last chain
        _ -> Epic.fail $ species ++ " has multiple final evolutions: " ++
         List.intercalate ", " (map (Util.toLower . fst . List.last) chains)
  return evolution

-- This tests eligibility for things like Little Cup where the species
-- must have an evolution but must not be an evolution itself.
--
isFirstEvolution :: GameMaster -> PokemonBase  -> Bool
isFirstEvolution gameMaster base =
  PokemonBase.hasEvolutions base &&
    (not $ isEvolution gameMaster base)

isEvolution :: GameMaster -> PokemonBase -> Bool
isEvolution gameMaster base =
  let allBases = GameMaster.allPokemonBases gameMaster
      getSpecies (species, _, _) = species
      stripNormal string = case Data.List.Extra.stripSuffix "_normal" string of
        Just newString -> newString
        Nothing -> string
      allEvolvedSpecies =
        -- XXX it's a bit of a mess what's upper and what's lower.
        map (stripNormal . Util.toLower . getSpecies) $
          concat $ map PokemonBase.evolutions allBases
  in PokemonBase.species base `elem` allEvolvedSpecies

-- If species has a normal form then species will have _NORMAL here.
-- Allow a match with either targete or target_NORMAL so the user can
-- specify either.
matchWithNormal :: String -> String -> Bool
matchWithNormal target species =
  species `elem` [target, target ++ "_normal"]

evolutionChains ::
  Epic.MonadCatch m => GameMaster -> Bool -> (String, Int) ->
    m [[(String, Int)]]
evolutionChains gameMaster traded (species, candy) = do
  base <- GameMaster.getPokemonBase gameMaster species
  case PokemonBase.evolutions base of
    [] -> return [[(species, candy)]]
    evolutions -> do
      concat <$> (mapM (\ (evolution, candy', noCandyCostViaTrade) -> do
          rest <- evolutionChains gameMaster traded (evolution, candy +
            if noCandyCostViaTrade && traded then 0 else candy')
          return $ map ((species, candy):) rest))
        evolutions
