module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified BattleState
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import           Move (Move)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Type
import           Type (Type)

import qualified Data.List as List
import qualified System.IO as I
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  glass    :: Bool,
  quick    :: Bool,
  level    :: Maybe Float,
  attackerSource :: AttackerSource,
  defender :: String
}

data Attacker = Attacker String (Maybe Float)

data AttackerSource =
    FromFile FilePath
  | AllAttackers
  | MovesetFor [Attacker]

defaultFilename = "my_pokemon.yaml"
defaultAttackerLevel = 20

parseAttacker :: O.ReadM Attacker
parseAttacker = O.eitherReader $ \s ->
  let splitRegex = Regex.mkRegex "^([a-z]+)(:([0-9]+(\\.5)?))?$"
  in case Regex.matchRegex splitRegex s of
       Just [species, _, "", _] ->
         Right $ Attacker species Nothing
       Just [species, _, levelString, _] ->
         Right $ Attacker species (Just $ read levelString)
       _ ->
         Left $ "`" ++ s ++ "' should look like ATTACKER[:LEVEL]"

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optGlass <*> optQuick <*> optLevel <*>
        optAttackerSource <*> optDefender
      optGlass = O.switch
        (  O.long "glass"
        <> O.short 'g'
        <> O.help "Sort output by dps to find glass cannons")
      optQuick = O.switch
        (  O.long "quick"
        <> O.short 'q'
        <> O.help "Use quick moves only")
      optLevel = O.optional $ O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.metavar "LEVEL"
        <> O.help ("Force my_pokemon level to find who's implicitly best, " ++
             "or set the level for -a or the default level for -m"))
      optAttackerSource = optFilename <|> optAll <|> optMovesetFor
      optFilename = FromFile <$> O.strOption
        (  O.long "file"
        <> O.short 'f'
        <> O.value defaultFilename
        <> O.showDefault
        <> O.metavar "FILE"
        <> O.help "File to read my_pokemon from")
      optAll = O.flag' AllAttackers
        (  O.long "all"
        <> O.short 'a'
        <> O.help "Consider all pokemon, not just the ones in FILE")
      optMovesetFor = MovesetFor <$> (O.some . O.option parseAttacker)
        (  O.long "moveset"
        <> O.short 'm'
        <> O.metavar "ATTACKER[:LEVEL]"
        <> O.help "Rate the movesets for ATTACKER against DEFENDER")
      optDefender = O.argument O.str (O.metavar "DEFENDER")
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

      gameMaster <- do
        ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
        ioGameMaster

      -- XXX Replacing "defender" with "_" shows the compiler knows
      -- defender must be Options -> String.  Too bad this needs a type
      -- annotation to disambiguate.

      defender <- do
        defenderBase <- GameMaster.getPokemonBase gameMaster $
          defender options
        return $ makeDefenderFromBase gameMaster defenderBase

      pokemon <- case attackerSource options of
        FromFile filename -> do
          myPokemon <- do
            ioMyPokemon <- MyPokemon.load filename
            ioMyPokemon
          mapM (makePokemon gameMaster (level options)) myPokemon
        AllAttackers ->
          return $ allAttackers gameMaster (attackerLevel options)
        MovesetFor attackers ->
          let attackerSpecies = map (\ (Attacker species _) -> species) attackers
          in case filter (not . GameMaster.isSpecies gameMaster) attackerSpecies of
               [] -> makeSomeAttackers gameMaster attackers (attackerLevel options)
               noSuchSpecies -> Epic.fail $ "No such species: " ++ (List.intercalate ", " noSuchSpecies)

      let useCharge = not $ quick options
          results = map (counter useCharge defender) pokemon
          sorted = reverse $ flip List.sortBy results $
            if glass options then byDps else byExpecteds

      mapM_ putStrLn $ map showResult sorted
    )
    $ \ex -> I.hPutStrLn I.stderr ex

attackerLevel :: Options -> Float
attackerLevel options =
  maybe defaultAttackerLevel id (level options)

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
          format string ((floor expected) :: Int) (stars expected))
      expecteds

data Result = Result {
  name      :: String,
  dps       :: Float,
  expecteds :: [(String, Float)]
} deriving (Show)

makePokemon :: Epic.MonadCatch m => GameMaster -> Maybe Float -> MyPokemon -> m Pokemon
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
            species ++ " can't do " ++ string ++ " move " ++
              MyPokemon.quickName myPokemon
  quick <- do
    quick <- getMove "quick" GameMaster.getQuick MyPokemon.quickName
      PokemonBase.quickMoves
    maybeSetHiddenPowerType gameMaster quick
      (MyPokemon.hiddenPowerType myPokemon)
  charge <- getMove "charge"
    GameMaster.getCharge MyPokemon.chargeName PokemonBase.chargeMoves

  let types = PokemonBase.types base

  cpMultiplier <- do
    level <- case maybeLevel of
      Nothing -> MyPokemon.level myPokemon
      Just val -> return $ val
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
  in map (\moveType -> (Type.name moveType, expected moveType))
        moveTypes

makeExpected :: Pokemon -> Pokemon -> Type -> Float
makeExpected defender attacker moveType =
  (fromIntegral $ Pokemon.hp attacker) *
    (Pokemon.defense attacker) /
    ((Type.stabFor moveType $ Pokemon.types defender) *
     (Type.effectivenessAgainst moveType $ Pokemon.types attacker)) / 1000

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

allAttackers :: GameMaster -> Float -> [Pokemon]
allAttackers gameMaster level =
  concat $ map (makeAllAttackersFromBase gameMaster level) $
    GameMaster.allPokemonBases gameMaster

makeAllAttackersFromBase :: GameMaster -> Float -> PokemonBase ->[Pokemon]
makeAllAttackersFromBase gameMaster level base =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      makeStat baseStat = (fromIntegral baseStat + 11) * cpMultiplier
      makeAttacker quickMove chargeMove =
        let speciesAndLevel :: String
            speciesAndLevel = Printf.printf "%s:%f" (PokemonBase.species base) level
            speciesAndLevel' = Regex.subRegex (Regex.mkRegex "\\.0") speciesAndLevel ""
            format = Printf.printf "%-15s %-13s/ %-15s"
            name = format speciesAndLevel' (Move.name quickMove) (Move.name chargeMove)
        in Pokemon.new
             name
             (PokemonBase.species base)
             (PokemonBase.types base)
             (makeStat $ PokemonBase.attack base)
             (makeStat $ PokemonBase.defense base)
             (makeStat $ PokemonBase.stamina base)
             quickMove
             chargeMove
             base
  in [makeAttacker quickMove chargeMove |
      quickMove <- PokemonBase.quickMoves base,
      chargeMove <- PokemonBase.chargeMoves base]

makeSomeAttackers :: (Epic.MonadCatch m) => GameMaster -> [Attacker] -> Float -> m [Pokemon]
makeSomeAttackers gameMaster attackers defaultLevel = do
  concat <$> mapM (\ (Attacker species level) -> do
    let level' = maybe defaultLevel id level
    base <- GameMaster.getPokemonBase gameMaster species
    return $ makeAllAttackersFromBase gameMaster level' base)
    attackers

maybeSetHiddenPowerType :: (Epic.MonadCatch m) =>
    GameMaster -> Move -> Maybe String -> m Move
maybeSetHiddenPowerType gameMaster move maybeTypeName = do
  if Move.isHiddenPower move
    then case maybeTypeName of
      Just typeName -> do
        moveType <- GameMaster.getType gameMaster typeName
        return $ Move.setType move moveType
      Nothing -> Epic.fail $ "No type given for hidden power"
    else case maybeTypeName of
      Nothing -> return $ move
      _ -> Epic.fail $ (Move.name move) ++ " does not take a type"
