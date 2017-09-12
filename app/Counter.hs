{-# LANGUAGE DuplicateRecordFields #-}

import qualified System.Environment

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Move
import           Move (Move)
import           Type (Type)

main = do
  Epic.catch (
    do
      ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
      gameMaster <- ioGameMaster

      args <- System.Environment.getArgs
      species <- case args of
        [species] -> return species
        _ -> Epic.fail "usage: counter species"
      defenderBase <- GameMaster.getPokemonBase gameMaster species

      ioMyPokemon <- MyPokemon.load "my_pokemon.yaml"
      myPokemon <- ioMyPokemon
      pokemon <- mapM (makePokemon gameMaster) myPokemon

      let results = map (counter defenderBase) pokemon

      return ())
    (\ex -> putStrLn $ "oops: " ++ (show ex))

data Pokemon = Pokemon {
  name        :: String,
  species     :: String,
  types       :: [Type],
  attack      :: Float,
  defense     :: Float,
  stamina     :: Float,
  quick       :: Move,
  charge      :: Move
} deriving (Show)

hp :: Pokemon -> Integer
hp this = floor $ stamina this

data Result = Result {
  name     :: String,
  dps      :: Float,
  totals   :: [(String, Float)]
} deriving (Show)

makePokemon :: Epic.MonadCatch m => GameMaster -> MyPokemon -> m Pokemon
makePokemon gameMaster myPokemon = do
  let name = MyPokemon.name myPokemon
      species = MyPokemon.species myPokemon

  let fromGameMaster getFunc keyFunc = getFunc gameMaster $ keyFunc myPokemon

  base <- fromGameMaster GameMaster.getPokemonBase MyPokemon.species

  let getMove string getFunc keyFunc moveListFunc = do
        move <- fromGameMaster getFunc keyFunc
        case move `elem` moveListFunc base of
          True -> return move
          False -> Epic.fail $
            species ++ "can't do " ++ string ++ " move " ++
              MyPokemon.quickName myPokemon
  quick <- getMove "quick"
    GameMaster.getQuick MyPokemon.quickName PokemonBase.quickMoves
  charge <- getMove "charge"
    GameMaster.getCharge MyPokemon.chargeName PokemonBase.chargeMoves

  let types = PokemonBase.types base

  cpMultiplier <- do
    level <- MyPokemon.level myPokemon
    return $ GameMaster.getCpMultiplier gameMaster level

  let getStat getBaseStat getMyStat = do
        let baseStat = getBaseStat base
        myStat <- getMyStat myPokemon
        return $ fromIntegral (baseStat + myStat) * cpMultiplier

  attack <- getStat PokemonBase.attack MyPokemon.attack
  defense <- getStat PokemonBase.defense MyPokemon.defense
  stamina <- getStat PokemonBase.stamina MyPokemon.stamina

  return $ Pokemon name species types attack defense stamina quick charge

counter :: PokemonBase -> Pokemon -> Result
counter defenderBase pokemon =
  _


damage :: Move -> [Type] -> [Type] -> Float
damage move attackerTypes defenderTypes =
  let stab = Move.stabFor move attackerTypes
      effectiveness = Move.effectivenessAgainst move defenderTypes
  in (Move.power move) * stab * effectiveness

calcDps :: Move -> [Type] -> [Type] -> Float
calcDps move attackerTypes defenderTypes =
  damage move attackerTypes defenderTypes / Move.duration move

-- List.sortBy byDps results
-- reverse $ List.sortBy byDps results
-- (List.sortBy byDps) results
-- (reverse . List.sortBy byDps) results

byDps :: Result -> Result -> Ordering
byDps first second =
  (dps first) `compare` (dps second) 

byTotals :: Result -> Result -> Ordering
byTotals first second =
  let min result = minimum $ map snd $ totals result
  in min first `compare` min second
