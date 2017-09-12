{-# LANGUAGE DuplicateRecordFields #-}

import qualified System.Environment

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
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
  quick <- fromGameMaster GameMaster.getQuick MyPokemon.quickName
  charge <- fromGameMaster GameMaster.getCharge MyPokemon.chargeName

  let types = PokemonBase.types base

--  cpMultiplier <- MyPokemon.level myPokemon

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
