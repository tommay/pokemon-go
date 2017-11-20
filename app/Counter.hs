{-# LANGUAGE OverloadedStrings #-}

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
import qualified Mythical
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Type
import           Type (Type)

import           Control.Applicative (optional, some)
import qualified Data.Attoparsec.Text as AP
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.IO as I
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  glass    :: Bool,
  dpsFilter :: Maybe Int,
  top      :: Maybe Int,
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
  let attoParseAttacker = do
        attacker <- some $ AP.notChar ':'
        level <- optional $ do
          AP.char ':'
          (level, _) <- AP.match $ do
            AP.decimal
            optional $ AP.string ".5"
          return $ read $ Text.unpack level
        AP.endOfInput
        return $ Attacker attacker level
  in case AP.parseOnly attoParseAttacker (Text.pack s) of
    Left _ ->
      Left $ "`" ++ s ++ "' should look like ATTACKER[:LEVEL]"
    Right attacker -> Right attacker

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optGlass <*> optDpsFilter <*>
        optTop <*> optQuick <*>
        optLevel <*> optAttackerSource <*> optDefender
      optGlass = O.switch
        (  O.long "glass"
        <> O.short 'g'
        <> O.help "Sort output by dps to find glass cannons")
      optDpsFilter = O.optional $ O.option O.auto
        (  O.long "dps"
        <> O.short 'd'
        <> O.metavar "N"
        <> O.help "Filter pokemon to the top 1/N by DPS")
      optTop = O.optional $ O.option O.auto
        (  O.long "top"
        <> O.short 't'
        <> O.metavar "N"
        <> O.help "Show the top N attacker species")
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
        AllAttackers -> do
          mythicalMap <- do
            ioMythicalMap <- Mythical.load "mythical.yaml"
            ioMythicalMap
          return $
            filter (not . Mythical.isMythical mythicalMap . Pokemon.species)
            $ allAttackers gameMaster (attackerLevel options)
        MovesetFor attackers ->
          let attackerSpecies = map (\ (Attacker species _) -> species) attackers
          in case filter (not . GameMaster.isSpecies gameMaster) attackerSpecies of
               [] -> makeSomeAttackers gameMaster attackers (attackerLevel options)
               noSuchSpecies -> Epic.fail $ "No such species: " ++ (List.intercalate ", " noSuchSpecies)

      let useCharge = not $ quick options
          results = map (counter useCharge defender) pokemon
          sortedByDps = List.reverse $ List.sortBy byDps results
          sorted = if glass options
            then sortedByDps
            else List.reverse $ List.sortBy byExpecteds results
          filtered = case dpsFilter options of
            Just n ->
              let dpsCutoff = dps $ sortedByDps !! (length sortedByDps `div` n) 
                  dpsCutoffNames = Set.fromList $
                    map (\r -> Pokemon.pname (Main.pokemon r)) $
                    filter (\r -> dps r >= dpsCutoff) results
              in filter (\r -> Pokemon.pname (Main.pokemon r) `elem` dpsCutoffNames) sorted
            Nothing -> sorted
          nameFunc = case attackerSource options of
            FromFile _ -> nameName
            AllAttackers -> nameSpeciesAndLevelAndMoveset
            MovesetFor _ -> nameSpeciesAndLevelAndMoveset

      case top options of
        Just n ->
          let topSpecies = take n $ List.nub $
                map (\r -> Pokemon.species $ Main.pokemon r) filtered
          in mapM_ putStrLn topSpecies
        Nothing ->
          mapM_ (putStrLn . showResult nameFunc) filtered
    )
    $ \ex -> I.hPutStrLn I.stderr ex

attackerLevel :: Options -> Float
attackerLevel options =
  Maybe.fromMaybe defaultAttackerLevel (level options)

showResult :: (Pokemon -> String) -> Result -> String
showResult nameFunc result =
  let format = Printf.printf "%4.1f %-12s"
  in format (dps result) (nameFunc $ pokemon result) ++ "   " ++
       showExpecteds (expecteds result)

showExpecteds :: [(String, Float)] -> String
showExpecteds expecteds =
  let format = Printf.printf "%s:%-3d %-6s"
      stars n = replicate (floor $ n / 70) '*'
  in List.intercalate " " $
    map (\ (string, expected) ->
          format string ((floor expected) :: Int) (stars expected))
      expecteds

nameName :: Pokemon -> String
nameName pokemon =
  Pokemon.pname pokemon

nameSpeciesAndLevelAndMoveset :: Pokemon -> String
nameSpeciesAndLevelAndMoveset pokemon =
  let speciesAndLevel = Printf.printf "%s:%f"
        (Pokemon.species pokemon)
        (Pokemon.level pokemon)
      speciesAndLevel' = Regex.subRegex (Regex.mkRegex "\\.0$") speciesAndLevel ""
      format = Printf.printf "%-15s %-13s/ %-15s"
  in format speciesAndLevel'
       (Move.name $ Pokemon.quick pokemon)
       (Move.name $ Pokemon.charge pokemon)

data Result = Result {
  pokemon   :: Pokemon,
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

  level <- case maybeLevel of
    Nothing -> MyPokemon.level myPokemon
    Just val -> return $ val

  let types = PokemonBase.types base

  let cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      getStat getBaseStat getMyStat = do
        let baseStat = getBaseStat base
        myStat <- getMyStat myPokemon
        return $ fromIntegral (baseStat + myStat) * cpMultiplier

  attack <- getStat PokemonBase.attack MyPokemon.attack
  defense <- getStat PokemonBase.defense MyPokemon.defense
  stamina <- getStat PokemonBase.stamina MyPokemon.stamina

  return $ Pokemon.new name species level types attack defense stamina quick charge base

counter :: Bool -> Pokemon -> Pokemon -> Result
counter useCharge defender attacker =
  let dps = BattleState.calcDps attacker defender useCharge
      expecteds = makeExpecteds dps defender attacker
  in Result attacker dps expecteds

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
    level
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
        Pokemon.new
          (PokemonBase.species base)
          (PokemonBase.species base)
          level
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
    let level' = Maybe.fromMaybe defaultLevel level
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
