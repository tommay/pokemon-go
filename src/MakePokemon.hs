module MakePokemon (
  makePokemon
) where

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import qualified Move
import           Move (Move)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase

import qualified Text.Regex as Regex

makePokemon :: Epic.MonadCatch m =>
  GameMaster -> (Float -> Float) -> MyPokemon -> m [Pokemon]
makePokemon gameMaster tweakLevel myPokemon = do
  let name = MyPokemon.name myPokemon
      species = MyPokemon.species myPokemon

      fromGameMaster getFunc keyFunc = getFunc gameMaster $ keyFunc myPokemon

  base <- fromGameMaster GameMaster.getPokemonBase MyPokemon.species

  let types = PokemonBase.types base

      getMove moveType getFunc keyFunc moveListFunc = do
        move <- fromGameMaster getFunc keyFunc
        let moveList = moveListFunc base
        if move `elem` moveList
          then return move
          else Epic.fail $ species ++ " can't do " ++ moveType ++ " move " ++
                 keyFunc myPokemon

  quick <- do
    let split = Regex.mkRegex "([^,]*)(, *(.*))?"
        Just [quickName, _, extra] =
          Regex.matchRegex split $ MyPokemon.quickName myPokemon
    quick <- getMove quickName GameMaster.getQuick (const quickName)
      PokemonBase.quickMoves
    maybeSetHiddenPowerType gameMaster quick extra

  charge <- getMove "charge"
    GameMaster.getCharge MyPokemon.chargeName PokemonBase.chargeMoves

  ivs <- case MyPokemon.ivs myPokemon of
    Just ivs@(_:_) -> return ivs
    _ -> Epic.fail $ "No ivs for " ++ MyPokemon.name myPokemon

  let makeForIvs ivs =
        let level = tweakLevel $ IVs.level ivs
            cpMultiplier = GameMaster.getCpMultiplier gameMaster level
            getStat getBaseStat getIv =
              let baseStat = getBaseStat base
                  iv = getIv ivs
              in fromIntegral (baseStat + iv) * cpMultiplier
            attack = getStat PokemonBase.attack IVs.attack
            defense = getStat PokemonBase.defense IVs.defense
            stamina = getStat PokemonBase.stamina IVs.stamina
        in Pokemon.new name base ivs attack defense stamina quick charge

  return $ map makeForIvs ivs

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
