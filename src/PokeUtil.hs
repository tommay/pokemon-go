module PokeUtil (
  addStats,
  setLevel,
  setMoves,
  levelToString,
  evolveFully,
  evolveFullyWithCandy,
  evolveSpeciesFullyWithCandy,
  evolutionChains
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
    evolveSpeciesFullyWithCandy gameMaster maybeTarget species
  return $ (MyPokemon.setSpecies myPokemon evolvedSpecies, candy)

-- If there is a target, return it from all the evolutions or fail
-- if it's not an evolution.
-- Otherwise return the final evolution if there is only one chain.
--
evolveSpeciesFullyWithCandy :: Epic.MonadCatch m =>
  GameMaster -> Maybe String -> String -> m (String, Int)
evolveSpeciesFullyWithCandy gameMaster maybeTarget species = do
  chains <- evolutionChains gameMaster (species, 0)
  evolution <- case maybeTarget of
    Just target ->
      case List.find (matchWithNormal (Util.toLower target) .
          Util.toLower . fst) (concat chains) of
        Nothing -> Epic.fail $ species ++ " does not evolve to " ++ target
        Just evolution -> return evolution
    Nothing ->
      case chains of
        [chain] -> return $ List.last chain
        _ -> Epic.fail $ species ++ " has multiple final evolutions"
  return evolution

-- If species has a normal form then species will have _NORMAL here.
-- Allow a match with either targete or target_NORMAL so the user can
-- specify either.
matchWithNormal :: String -> String -> Bool
matchWithNormal target species =
  species `elem` [target, target ++ "_normal"]

evolutionChains ::
  Epic.MonadCatch m => GameMaster -> (String, Int) -> m [[(String, Int)]]
evolutionChains gameMaster (species, candy) = do
  base <- GameMaster.getPokemonBase gameMaster species
  case PokemonBase.evolutions base of
    [] -> return [[(species, candy)]]
    evolutions -> do
      concat <$> (mapM (\ (evolution, candy') -> do
          rest <- evolutionChains gameMaster (evolution, candy + candy')
          return $ map ((species, candy):) rest))
        evolutions
