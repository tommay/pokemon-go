module Main where

import           Data.Char (toLower)
import qualified Data.List as List
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex
import qualified System.Environment

import Options.Applicative as O
import Data.Semigroup ((<>))

import qualified BattleState
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Move
import           Move (Move)
import qualified Type
import           Type (Type)

data Options = Options {
  glass    :: Bool,
  quick    :: Bool,
  level    :: Integer,
  species  :: String
}

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optGlass <*> optQuick <*> optLevel <*> optSpecies
      optGlass = O.switch
        (  O.long "glass"
        <> O.short 'g'
        <> O.help "Sort output by dps to find glass cannons")
      optQuick = O.switch
        (  O.long "quick"
        <> O.short 'q'
        <> O.help "Use quick moves only")
      optLevel = O.option auto
        (  O.long "level"
        <> O.short 'l'
        <> O.value 0
        <> O.metavar "LEVEL"
        <> O.help "Force my_pokemon level to find who's implicitly best")
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Find good counters for a Pokemon."
        <> O.header "header - Find good counters for a Pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  O.customExecParser prefs options

main = do
  Epic.catch (
    do
      options <- getOptions

      ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
      gameMaster <- ioGameMaster

      -- XXX Replacing "species" with "_" shows the compiler knows
      -- species must be Options -> String.  Too bad this needs a type
      -- annotation to disambiguate.

      defender <- do
        defenderBase <- GameMaster.getPokemonBase gameMaster $
          (species :: Options -> String) options
        return $ makeDefenderFromBase gameMaster defenderBase

      let maybeLevel = case level options of
            0 -> Nothing
            val -> Just val

      ioMyPokemon <- MyPokemon.load "my_pokemon.yaml"
      myPokemon <- ioMyPokemon
      pokemon <- mapM (makePokemon gameMaster maybeLevel) myPokemon

      let useCharge = not $ quick options
          results = map (counter useCharge defender) pokemon
          sorted = reverse $ flip List.sortBy results $
            if glass options then byDps else byExpecteds

      mapM_ putStrLn $ map showResult sorted
    )
    (\ex -> putStrLn $ "oops: " ++ (show ex))

showResult :: Result -> String
showResult result =
  let format = Printf.printf "%4.1f %-12s"
  in format (dps result) (name result) ++ "   " ++
       showExpecteds (expecteds result)

showExpecteds :: [(String, Float)] -> String
showExpecteds expecteds =
  let format = Printf.printf "%s:%-3d %-6s"
      stars n = replicate (floor $ n / 70) '*'
  in List.intercalate " " $
    map (\ (string, expected) ->
          format string ((floor expected) :: Integer) (stars expected))
      expecteds

data Result = Result {
  name      :: String,
  dps       :: Float,
  expecteds :: [(String, Float)]
} deriving (Show)

makePokemon :: Epic.MonadCatch m => GameMaster -> Maybe Integer -> MyPokemon -> m Pokemon
makePokemon gameMaster maybeLevel myPokemon = do
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
    level <- case maybeLevel of
      Nothing -> MyPokemon.level myPokemon
      Just val -> return $ fromIntegral val
    return $ GameMaster.getCpMultiplier gameMaster level

  let getStat getBaseStat getMyStat = do
        let baseStat = getBaseStat base
        myStat <- getMyStat myPokemon
        return $ fromIntegral (baseStat + myStat) * cpMultiplier

  attack <- getStat PokemonBase.attack MyPokemon.attack
  defense <- getStat PokemonBase.defense MyPokemon.defense
  stamina <- getStat PokemonBase.stamina MyPokemon.stamina

  return $ Pokemon.new name species types attack defense stamina quick charge base

counter :: Bool -> Pokemon -> Pokemon -> Result
counter useCharge defender attacker =
  let dps = BattleState.calcDps attacker defender useCharge
      expecteds = makeExpecteds dps defender attacker
      name' = Pokemon.pname attacker
  in Result name' dps expecteds

makeExpecteds :: Float -> Pokemon -> Pokemon -> [(String, Float)]
makeExpecteds dps defender attacker =
  let moveTypes = sortMoveTypes defender $ getMoveTypes defender
      expected moveType = dps * makeExpected defender attacker moveType
  in map (\moveType ->
           (simplify $ Type.name moveType, expected moveType)) moveTypes

makeExpected :: Pokemon -> Pokemon -> Type -> Float
makeExpected defender attacker moveType =
  (fromIntegral $ Pokemon.hp attacker) *
    (Pokemon.defense attacker) /
    ((Type.stabFor moveType $ Pokemon.types defender) *
     (Type.effectivenessAgainst moveType $ Pokemon.types attacker)) / 1000

simplify :: String -> String
simplify name =
  let regex = Regex.mkRegex ".*_"
  in map toLower $ Regex.subRegex regex name ""

getSortedMoveTypes :: Pokemon -> [Type]
getSortedMoveTypes pokemon =
  sortMoveTypes pokemon $ getMoveTypes pokemon

getMoveTypes :: Pokemon -> [Type]
getMoveTypes pokemon =
  List.nub $ map Move.moveType $ Pokemon.possibleMoves pokemon

-- Sort the move types so the ones with stab are at the front.
--
sortMoveTypes :: Pokemon -> [Type] -> [Type]
sortMoveTypes pokemon ptypes =
  let pokemonTypes = Pokemon.types pokemon
      hasStab ptype = ptype `elem` pokemonTypes
      (stab, noStab) = List.partition hasStab ptypes
  in stab ++ noStab

-- List.sortBy byDps results
-- reverse $ List.sortBy byDps results
-- (List.sortBy byDps) results
-- (reverse . List.sortBy byDps) results
-- reverse . List.sortBy byDps $ results

byDps :: Result -> Result -> Ordering
byDps first second =
  (dps first) `compare` (dps second) 

byExpecteds :: Result -> Result -> Ordering
byExpecteds first second =
  let min result = minimum $ map snd $ expecteds result
  in min first `compare` min second

makeDefenderFromBase :: GameMaster -> PokemonBase -> Pokemon
makeDefenderFromBase gameMaster base =
  let level = 20
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      maxStat baseStat = (fromIntegral baseStat + 15) * cpMultiplier
  in Pokemon.new
    (PokemonBase.species base)
    (PokemonBase.species base)
    (PokemonBase.types base)
    (maxStat $ PokemonBase.attack base)
    (maxStat $ PokemonBase.defense base)
    (maxStat $ PokemonBase.stamina base)
    (List.head $ PokemonBase.quickMoves base)
    (List.head $ PokemonBase.chargeMoves base)
    base
