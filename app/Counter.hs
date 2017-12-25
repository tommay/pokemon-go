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
import           Weather (Weather (..))

import           Control.Applicative (optional, some)
import           Control.Monad (join)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.IO as I
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  maybeWeather :: Maybe Weather,
  sortOutputBy :: SortOutputBy,
  dpsFilter :: Maybe Int,
  top      :: Maybe Int,
  level    :: Maybe Float,
  legendary :: Bool,
  attackerSource :: AttackerSource,
  defender :: String
} deriving (Show)

data Attacker = Attacker String (Maybe Float)
  deriving (Show)

data SortOutputBy =
  ByDamage | ByDps | ByProduct | ByDamagePerHp | Weighted Float
  deriving (Show)

data AttackerSource =
    FromFiles [FilePath]
  | AllAttackers
  | MovesetFor [Attacker]
  deriving (Show)

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
        attacker <- some $ Atto.notChar ':'
        level <- optional $ do
          Atto.char ':'
          (level, _) <- Atto.match $ do
            Atto.decimal
            optional $ Atto.string ".5"
          return $ read $ Text.unpack level
        Atto.endOfInput
        return $ Attacker attacker level
  in case Atto.parseOnly attoParseAttacker (Text.pack s) of
    Left _ ->
      Left $ "`" ++ s ++ "' should look like ATTACKER[:LEVEL]"
    Right attacker -> Right attacker

getOptions :: IO Options
getOptions =
  let opts = Options <$> optWeather <*> optSortOutputBy <*> optDpsFilter <*>
        optTop <*>
        optLevel <*> optLegendary <*> optAttackerSource <*>
        optDefender
      optWeather = O.optional $
            O.flag' Clear (O.long "clear" <> O.help "weather is sunny/clear")
        <|> O.flag' Fog (O.long "fog")
        <|> O.flag' Overcast (O.long "overcast")
        <|> O.flag' PartlyCloudy (O.long "partlycloudy")
        <|> O.flag' Rainy (O.long "rainy")
        <|> O.flag' Snow (O.long "snow")
        <|> O.flag' Windy (O.long "windy")
      optSortOutputBy = optGlass <|> optProduct <|> optDamagePerHp
        <|> optWeighted <|> pure ByDamage
      optGlass = O.flag' ByDps
        (  O.long "glass"
        <> O.short 'g'
        <> O.help "Sort output by dps to find glass cannons")
      optProduct = O.flag' ByProduct
        (  O.long "product"
        <> O.short 'p'
        <> O.help "Sort output by product of dps and damage")
      optDamagePerHp = O.flag' ByDamagePerHp
        (  O.short 'h'
        <> O.help "Sort output by damage per hp")
      optWeighted = Weighted <$> O.option O.auto
        (  O.long "weighted"
        <> O.short 'w'
        <> O.metavar "WEIGHT"
        <> O.help ("Sort by weighted mix of dps and damage, where weight " ++
             "varies from 0.0 for pure dps to 1.0 for pure damage"))
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
      optAttackerSource = optFilenames <|> optMovesetFor <|> optAll
        <|> (pure $ FromFiles [defaultFilename])
      optFilenames = FromFiles <$> (O.some . O.strOption)
        (  O.long "file"
        <> O.short 'f'
        <> O.metavar "FILE"
        <> O.help ("File to read my_pokemon from (defauult: " ++
             defaultFilename ++ ")"))
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

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      defenderVariants <- do
        defenderBase <- GameMaster.getPokemonBase gameMaster $
          defender options
        return $ makeWithAllMovesetsFromBase gameMaster defenderLevel
          defenderBase

      attackers <- case attackerSource options of
        FromFiles filenames -> do
          let loadPokemon filename = do
                myPokemon <- join $ MyPokemon.load filename
                mapM (makePokemon gameMaster (level options)) myPokemon
          fmap concat $ sequence $ map loadPokemon filenames
        AllAttackers -> do
          mythicalMap <- join $ Mythical.load "mythical.yaml"
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

      let weatherBonus = case maybeWeather options of
            Just weather -> GameMaster.getWeatherBonus gameMaster weather
            Nothing -> const 1
          results = map (counter weatherBonus defenderVariants) attackers
          sortedByDps = List.reverse $ Util.sortWith minDps results
          damagePerHp result = (fromIntegral $ minDamage result) /
            (fromIntegral $ Pokemon.hp $ pokemon result)
          sorted = case sortOutputBy options of
            ByDamage -> List.reverse $ Util.sortWith minDamage results
            ByDps -> sortedByDps
            ByProduct -> List.reverse $
              List.sortBy (Util.compareWith $ \result ->
                  minDps result * fromIntegral (minDamage result + 100))
                results
            ByDamagePerHp -> List.reverse $
              Util.sortWith damagePerHp results
            Weighted weight ->
              -- XXX Should this also subtract out dpsMin and damageMin?
              let damageMax = fromIntegral $ maximum $ map minDamage results
                  dpsMax = maximum $ map minDps results
                  weightedAverage result =
                    (fromIntegral $ minDamage result) / damageMax * weight +
                    (minDps result) / dpsMax * (1 - weight)
              in List.reverse $ Util.sortWith weightedAverage results

          filtered = case dpsFilter options of
            Just n ->
              let dpsCutoff = minDps $ sortedByDps !! (length sortedByDps `div` n) 
                  dpsCutoffNames = Set.fromList $
                    map (\r -> Pokemon.pname (Main.pokemon r)) $
                    filter (\r -> minDps r >= dpsCutoff) results
              in filter (\r -> Pokemon.pname (Main.pokemon r) `elem` dpsCutoffNames) sorted
            Nothing -> sorted
          nameFunc = case attackerSource options of
            FromFiles _ -> nameName
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
    $ I.hPutStrLn I.stderr

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

counter :: (Type -> Float) -> [Pokemon] -> Pokemon -> Result
counter weatherBonus defenderVariants attacker =
  let battles = [Battle.runBattleOnly $
        Battle.init weatherBonus attacker defender |
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
makeSomeAttackers gameMaster attackers defaultLevel =
  concat <$> mapM (\ (Attacker species level) -> do
    let level' = Maybe.fromMaybe defaultLevel level
    base <- GameMaster.getPokemonBase gameMaster species
    return $ makeAllAttackersFromBase gameMaster level' base)
    attackers

maybeSetHiddenPowerType :: (Epic.MonadCatch m) =>
    GameMaster -> Move -> Maybe String -> m Move
maybeSetHiddenPowerType gameMaster move maybeTypeName =
  if Move.isHiddenPower move
    then case maybeTypeName of
      Just typeName -> do
        moveType <- GameMaster.getType gameMaster typeName
        return $ Move.setType move moveType
      Nothing -> Epic.fail $ "No type given for hidden power"
    else case maybeTypeName of
      Nothing -> return $ move
      _ -> Epic.fail $ (Move.name move) ++ " does not take a type"
