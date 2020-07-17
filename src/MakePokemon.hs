module MakePokemon (
  makePokemon,
  makeForWhatevers,
  makeForWhatever,
  makeWithAllMovesetsFromBase,
) where

import qualified Discounts
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified Move
import           Move (Move)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)

import qualified Text.Regex as Regex

import qualified Debug as D

-- Turn each MyPokemon into a list of Pokemon, one for each of the
-- MyPokemon's charge moves.
--
makePokemon :: Epic.MonadCatch m => GameMaster -> MyPokemon -> m [Pokemon]
makePokemon gameMaster myPokemon = do
  let name = MyPokemon.name myPokemon
      species = MyPokemon.species myPokemon

      fromGameMaster getFunc keyFunc = getFunc gameMaster $ keyFunc myPokemon

  base <- fromGameMaster GameMaster.getPokemonBase MyPokemon.species

  let ivs = MyPokemon.ivs myPokemon

  let getMove moveType moveListFunc getFunc name = do
        move <- fromGameMaster getFunc (const name)
        let moveList = moveListFunc base
        if move `elem` moveList
          then return move
          else Epic.fail $ species ++ " can't do " ++ moveType ++ " move " ++
                 name

  quick <- do
    let split = Regex.mkRegex "([^,]*)(, *(.*))?"
        Just [quickName, _, extra] =
          Regex.matchRegex split $ MyPokemon.quickName myPokemon
    quick <- getMove "quick" PokemonBase.quickMoves
      GameMaster.getQuick quickName
    maybeSetHiddenPowerType gameMaster quick extra

  chargeMoves <- mapM (getMove "charge" PokemonBase.chargeMoves
    GameMaster.getCharge) $ MyPokemon.chargeNames myPokemon

  return $ makeForWhatevers gameMaster ivs name base [quick] chargeMoves

makeWithAllMovesetsFromBase :: GameMaster -> IVs -> PokemonBase -> [Pokemon]
makeWithAllMovesetsFromBase gameMaster ivs base =
  MakePokemon.makeForWhatevers
    gameMaster
    ivs
    (PokemonBase.species base)
    base
    (PokemonBase.quickMoves base)
    (PokemonBase.chargeMoves base)

makeForWhatevers ::
 GameMaster -> IVs -> String -> PokemonBase -> [Move] -> [Move] -> [Pokemon]
makeForWhatevers gameMaster ivs name base quickMoves chargeMoves =
  [makeForWhatever gameMaster ivs name base quick charge |
    charge <- chargeMoves, quick <- quickMoves]

makeForWhatever ::
  GameMaster -> IVs -> String -> PokemonBase -> Move -> Move -> Pokemon
makeForWhatever gameMaster ivs name base quick charge =
  let level = IVs.level ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      makeStat baseFunc ivFunc =
        (fromIntegral $ baseFunc base + ivFunc ivs) * cpMultiplier
  in Pokemon.new
       name
       base
       ivs
       (makeStat PokemonBase.attack IVs.attack)
       (makeStat PokemonBase.defense IVs.defense)
       (makeStat PokemonBase.stamina IVs.stamina)
       quick
       charge
       Discounts.noDiscounts

maybeSetHiddenPowerType :: (Epic.MonadCatch m) =>
    GameMaster -> Move -> String -> m Move
maybeSetHiddenPowerType gameMaster move typeName =
  if Move.isHiddenPower move
    then case typeName of
      "" -> Epic.fail $ "No type given for hidden power"
      _ -> do
        moveType <- GameMaster.getType gameMaster typeName
        return $ Move.setType move moveType
    else case typeName of
      "" -> return $ move
      _ -> Epic.fail $ (Move.name move) ++ " does not take a type"
