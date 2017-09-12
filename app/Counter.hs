{-# LANGUAGE DuplicateRecordFields #-}

import qualified System.Environment
import Data.List as List

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
          sorted = reverse $ List.sortBy byDps results

      mapM_ print sorted
    )
    (\ex -> putStrLn $ "oops: " ++ (show ex))

data Pokemon = Pokemon {
  pname       :: String,
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
counter defenderBase attacker =
  let move = {-Pokemon.-}quick attacker
      dps = damagePerSecond move attacker defenderBase
      totals = []
      name' = {-Pokemon.-}pname attacker
  in Result name' dps totals

damage :: Move -> Pokemon -> PokemonBase -> Integer
damage move attacker defenderBase =
  let stab = Move.stabFor move $ {-Pokemon.-}types attacker
      effectiveness = Move.effectivenessAgainst move $
        PokemonBase.types defenderBase
      attack' = {-Pokemon.-}attack attacker
      defense = fromIntegral $ PokemonBase.defense defenderBase + 15
      power = Move.power move
   in floor $ power * stab * effectiveness * attack' / defense / 2 + 1

damagePerSecond :: Move -> Pokemon -> PokemonBase -> Float
damagePerSecond move attacker defenderBase =
  fromIntegral (damage move attacker defenderBase) / Move.duration move

-- List.sortBy byDps results
-- reverse $ List.sortBy byDps results
-- (List.sortBy byDps) results
-- (reverse . List.sortBy byDps) results
-- reverse . List.sortBy byDps $ results

byDps :: Result -> Result -> Ordering
byDps first second =
  (dps first) `compare` (dps second) 

byTotals :: Result -> Result -> Ordering
byTotals first second =
  let min result = minimum $ map snd $ totals result
  in min first `compare` min second
