{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
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
import qualified Util

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
  level    :: Maybe Float,
  legendary :: Bool,
  attackerSource :: AttackerSource,
  defender :: String
}

data Attacker = Attacker String (Maybe Float)

data AttackerSource =
    FromFile FilePath
  | AllAttackers
  | MovesetFor [Attacker]

data Result = Result {
  pokemon   :: Pokemon,
  minDps       :: Float,
  minDamage :: Int
} deriving (Show)

defaultFilename = "my_pokemon.yaml"
defaultAttackerLevel = 20
defenderLevel = 20

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
getOptions =
  let opts = Options <$> optGlass <*> optDpsFilter <*>
        optTop <*>
        optLevel <*> optLegendary <*> optAttackerSource <*>
        optDefender
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
      optLegendary = O.flag True False
        (  O.long "legendary"
        <> O.short 'L'
        <> O.help "Exclude legendaries when using -a")
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
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- do
        ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
        ioGameMaster

      defenderVariants <- do
        defenderBase <- GameMaster.getPokemonBase gameMaster $
          defender options
        return $ makeWithAllMovesetsFromBase gameMaster defenderLevel
          defenderBase

      attackers <- case attackerSource options of
        FromFile filename -> do
          myPokemon <- do
            ioMyPokemon <- MyPokemon.load filename
            ioMyPokemon
          mapM (makePokemon gameMaster (level options)) myPokemon
        AllAttackers -> do
          mythicalMap <- do
            ioMythicalMap <- Mythical.load "mythical.yaml"
            ioMythicalMap
          let all = allAttackers gameMaster (attackerLevel options)
              notMythical = not . Mythical.isMythical mythicalMap . Pokemon.species
              notLegendary = not . Mythical.isLegendary mythicalMap . Pokemon.species
              nonMythical = filter notMythical all
              result = if legendary options
                then nonMythical
                else filter notLegendary nonMythical
          return result  
        MovesetFor attackers ->
          let attackerSpecies = map (\ (Attacker species _) -> species) attackers
          in case filter (not . GameMaster.isSpecies gameMaster) attackerSpecies of
               [] -> makeSomeAttackers gameMaster attackers (attackerLevel options)
               noSuchSpecies -> Epic.fail $ "No such species: " ++ (List.intercalate ", " noSuchSpecies)

      let results = map (counter defenderVariants) attackers
          sortedByDps = List.reverse $ Util.sortWith minDps results
          sorted = if glass options
            then sortedByDps
            else List.reverse $ Util.sortWith minDamage results
          filtered = case dpsFilter options of
            Just n ->
              let dpsCutoff = minDps $ sortedByDps !! (length sortedByDps `div` n) 
                  dpsCutoffNames = Set.fromList $
                    map (\r -> Pokemon.pname (Main.pokemon r)) $
                    filter (\r -> minDps r >= dpsCutoff) results
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
  Printf.printf "%4.1f %5d  %s"
    (minDps result) (minDamage result) (nameFunc $ pokemon result)

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

makeWithAllMovesetsFromBase gameMaster level base =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      makeStat baseStat = (fromIntegral baseStat + 11) * cpMultiplier
      makePokemon quickMove chargeMove =
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
  in [makePokemon quickMove chargeMove |
      (quickMove, chargeMove) <-
        PokemonBase.moveSets base]

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

counter :: [Pokemon] -> Pokemon -> Result
counter defenderVariants attacker =
  let battles = [Battle.runBattleOnly $ Battle.init attacker defender |
        defender <- defenderVariants]
      getMinValue fn = fn . List.minimumBy (Util.compareWith fn)
      minDamage = getMinValue Battle.damageInflicted battles
      minDps = getMinValue Battle.dps battles
  in Result attacker minDps minDamage

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
      (quickMove, chargeMove) <-
        PokemonBase.moveSets base]

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
