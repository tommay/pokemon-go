-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module GameMaster (
  GameMaster,
  load,
  getPokemonBase,
  getQuick,
  getCharge,
  getCpMultiplier,
  getLevelsForStardust,
  getStardustForLevel,
  allPokemonBases,
  getType,
  getWeatherBonus,
  isSpecies,
  dustAndLevel,
  candyAndLevel,
  allLevels,
  nextLevel,
) where

import qualified Epic
import qualified Move
import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import           StringMap (StringMap)
import qualified Type
import           Type (Type)
import qualified Weather
import           Weather (Weather (..))

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:), (.:?), (.!=))

import           Data.Text.Conversions (convertText)
import           Data.Char (toLower, toUpper)
import           Data.Hashable (Hashable)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector
import           Data.Vector (Vector, (!))
import qualified Text.Regex as Regex

import qualified Debug as D

data GameMaster = GameMaster {
  types         :: StringMap Type,
  moves         :: StringMap Move,
  pokemonBases  :: StringMap PokemonBase,
  cpMultipliers :: Vector Float,
  stardustCost  :: [Int],
  candyCost     :: [Int],
  weatherBonusMap :: HashMap Weather (HashMap Type Float)
} deriving (Show)

type ItemTemplate = Yaml.Object

new = GameMaster

load :: Epic.MonadCatch m => FilePath -> IO (m GameMaster)
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Left yamlParseException -> Epic.fail $ show yamlParseException
    Right yamlObject -> return $ makeGameMaster yamlObject

allPokemonBases :: GameMaster -> [PokemonBase]
allPokemonBases this =
  HashMap.elems $ pokemonBases this

getPokemonBase :: Epic.MonadCatch m => GameMaster -> String -> m PokemonBase
getPokemonBase this speciesName =
  GameMaster.lookup "species" (pokemonBases this) speciesName

getMove :: Epic.MonadCatch m => GameMaster -> String -> m Move
getMove this moveName  =
  GameMaster.lookup "move" (moves this) moveName

getQuick :: Epic.MonadCatch m => GameMaster -> String -> m Move
getQuick this moveName = do
  move <- getMove this (moveName ++ "_fast")
  case Move.isQuick move of
    True -> return move
    False -> Epic.fail $ moveName ++ " is not a quick move"

getCharge :: Epic.MonadCatch m => GameMaster -> String -> m Move
getCharge this moveName = do
  move <- getMove this moveName
  case Move.isCharge move of
    True -> return move
    False -> Epic.fail $ moveName ++ " is not a charge move"

getCpMultiplier :: GameMaster -> Float -> Float
getCpMultiplier this level =
  let cpMultipliers' = GameMaster.cpMultipliers this
      intLevel = floor level
  in case fromIntegral intLevel == level of
    True -> cpMultipliers' ! (intLevel - 1)
    False ->
      let cp0 = cpMultipliers' ! (intLevel - 1)
          cp1 = cpMultipliers' ! intLevel
      in sqrt $ (cp0*cp0 + cp1*cp1) / 2

costAndLevel :: [Int] -> [(Int, Float)]
costAndLevel costs =
  init $ concat $ map (\ (cost, level) -> [(cost, level), (cost, level + 0.5)])
    $ zip costs [1..]

dustAndLevel :: GameMaster -> [(Int, Float)]
dustAndLevel this =
  costAndLevel $ stardustCost this

candyAndLevel :: GameMaster -> [(Int, Float)]
candyAndLevel this =
  costAndLevel $ candyCost this

allLevels :: GameMaster -> [Float]
allLevels =
  map snd . dustAndLevel

nextLevel :: GameMaster -> Float -> Maybe (Int, Int, Float)
nextLevel this level =
  let candy = Maybe.fromJust $ candyAtLevel this level
      stardust = Maybe.fromJust $ dustAtLevel this level
  in case filter (> level) $ allLevels this of
       [] -> Nothing
       list -> Just (candy, stardust, minimum list)

getLevelsForStardust :: (Epic.MonadCatch m) => GameMaster -> Int -> m [Float]
getLevelsForStardust this starDust =
  case map snd $ filter (\(dust, _) -> dust == starDust) $ dustAndLevel this of
    [] -> Epic.fail $ "Bad dust amount: " ++ show starDust
    levels -> return levels

getStardustForLevel :: GameMaster -> Float -> Int
getStardustForLevel this level =
  fst $ head $ filter (\(_, lvl) -> lvl == level) $ dustAndLevel this

lookup :: Epic.MonadCatch m => String -> StringMap a -> String -> m a
lookup what hash key =
  case HashMap.lookup (sanitize key) hash of
    Just val -> return val
    Nothing -> Epic.fail $ "No such " ++ what ++ ": " ++ key

sanitize :: String -> String
sanitize string =
  let nonWordChars = Regex.mkRegex "\\W"
  in map toUpper $ Regex.subRegex nonWordChars string "_"

makeGameMaster :: Epic.MonadCatch m => Yaml.Object -> m GameMaster
makeGameMaster yamlObject = do
  itemTemplates <- getItemTemplates yamlObject
  types <- getTypes itemTemplates
  moves <- getMoves types itemTemplates
  pokemonBases <-
    makeObjects "pokemonSettings" getNameForPokemonBase
      (makePokemonBase types moves) itemTemplates
  cpMultipliers <- do
    playerLevel <- getFirst itemTemplates "playerLevel"
    getObjectValue playerLevel "cpMultiplier"
  stardustCost <- do
    pokemonUpgrades <- getFirst itemTemplates "pokemonUpgrades"
    getObjectValue pokemonUpgrades "stardustCost"
  candyCost <- do
    pokemonUpgrades <- getFirst itemTemplates "pokemonUpgrades"
    getObjectValue pokemonUpgrades "candyCost"
  weatherBonusMap <- do
    weatherBonusSettings <- getFirst itemTemplates "weatherBonusSettings"
    attackBonusMultiplier <-
      getObjectValue weatherBonusSettings "attackBonusMultiplier"
    weatherAffinityMap <-
      makeObjects "weatherAffinities" (getNameFromKey "weatherCondition")
        (makeWeatherAffinity types) itemTemplates
    return $
      foldr (\ (wstring, ptypes) map ->
        let weather = toWeather wstring
            m = foldr
              (\ ptype map -> HashMap.insert ptype attackBonusMultiplier map)
              HashMap.empty
              ptypes
        in HashMap.insert weather m map)
      HashMap.empty
      $ HashMap.toList weatherAffinityMap
  return $ GameMaster.new types moves pokemonBases cpMultipliers stardustCost
    candyCost weatherBonusMap

-- Here it's nice to use Yaml.Parser because it will error if we don't
-- get a [ItemTemplate], i.e., it checks that the Yaml.Values are the
-- correct type thanks to type inference.
--
getItemTemplates :: Epic.MonadCatch m => Yaml.Object -> m [ItemTemplate]
getItemTemplates yamlObject =
  toEpic $ Yaml.parseEither (.: "itemTemplates") yamlObject

getTypes :: Epic.MonadCatch m => [ItemTemplate] -> m (StringMap Type)
getTypes itemTemplates = do
  battleSettings <- getFirst itemTemplates "battleSettings"
  stab <- getObjectValue battleSettings "sameTypeAttackBonusMultiplier"
  makeObjects "typeEffective" (getNameFromKey "attackType")
    (makeType stab) itemTemplates

makeType :: Epic.MonadCatch m => Float -> ItemTemplate -> m Type
makeType stab itemTemplate = do
  attackScalar <- getObjectValue itemTemplate "attackScalar"
  let effectiveness = HashMap.fromList $ zip effectivenessOrder attackScalar
  name <- getObjectValue itemTemplate "attackType"
  return $ Type.new name effectiveness stab

effectivenessOrder :: [String]
effectivenessOrder =
  map (\ ptype -> "POKEMON_TYPE_" ++ map toUpper ptype)
    ["normal",
     "fighting",
     "flying",
     "poison",
     "ground",
     "rock",
     "bug",
     "ghost",
     "steel",
     "fire",
     "water",
     "grass",
     "electric",
     "psychic",
     "ice",
     "dragon",
     "dark",
     "fairy"]

getMoves :: Epic.MonadCatch m => StringMap Type -> [ItemTemplate] -> m (StringMap Move)
getMoves types itemTemplates =
  makeObjects "moveSettings" (getNameFromKey "movementId")
    (makeMove types) itemTemplates

makeMove :: Epic.MonadCatch m => StringMap Type -> ItemTemplate -> m Move
makeMove types itemTemplate =
  let getTemplateValue text = getObjectValue itemTemplate text
  in Move.new
    <$> getTemplateValue "movementId"
    <*> do
      typeName <- getTemplateValue "pokemonType"
      get types typeName
    <*> getObjectValueWithDefault itemTemplate "power" 0
    <*> ((/1000) <$> getTemplateValue "durationMs")
    <*> getTemplateValue "damageWindowStartMs"
    <*> getObjectValueWithDefault itemTemplate "energyDelta" 0

-- This is a little goofy because GAME_MASTER is a little goofy.  There are
-- three templates for the two exeggutor forms.  The firrst and third are
-- redundant:
--   pokemonId: EXEGGUTOR
-- and 
--   pokemonId: EXEGGUTOR
--   form: EXEGGUTOR_ALOLA
-- and
--   pokemonId: EXEGGUTOR
--   form: EXEGGUTOR_NORMAL
-- I want the original exeggutor to be known as just "exeggutor", and the
-- alola form to be known as "exeggutor_alola".  So, if there is a form
-- and the form does not end with "_NORMAL" then use it as the name, else
-- use the pokemonId.  This also works with DEOXYS.  Note that we'll add both
-- redundant templates to the pokemonBases hash, but the first one will
-- be overwritten which is find because except for the form the are identical.
--
getNameForPokemonBase :: Epic.MonadCatch m => ItemTemplate -> m String
getNameForPokemonBase itemTemplate =
  let mPokemonId = getObjectValue itemTemplate "pokemonId"
  in Epic.catch (do
       form <- getObjectValue itemTemplate "form"
       if "_NORMAL" `List.isSuffixOf` form
         then mPokemonId
         else return form)
     $ \ex -> mPokemonId

makePokemonBase :: Epic.MonadCatch m => StringMap Type -> StringMap Move -> ItemTemplate -> m PokemonBase
makePokemonBase types moves pokemonSettings =
  Epic.catch (do
    let getValue key = getObjectValue pokemonSettings key

    species <- do
      species <- getNameForPokemonBase pokemonSettings
      return $ map toLower species

    ptypes <- do
      ptype <- getValue "type"
      let ptypes = case getValue "type2" of
            Right ptype2 -> [ptype, ptype2]
            -- XXX This can swallow parse errors?
            Left _ -> [ptype]
      mapM (get types) ptypes
  
    statsObject <- getValue "stats"
    attack <- getObjectValue statsObject "baseAttack"
    defense <- getObjectValue statsObject "baseDefense"
    stamina <- getObjectValue statsObject "baseStamina"

    evolutions <- do
      case getValue "evolutionBranch" of
        Right evolutionBranch ->
          mapM (\ branch -> do
            evolution <- getObjectValue branch "evolution"
            candyCost <- getObjectValue branch "candyCost"
            return (evolution, candyCost))
            evolutionBranch
        -- XXX This can swallow parse errors?
        Left _ -> return []

    let getMoves key = do
          -- Mew has some moves specified multiple times so nub them.
          moveNames <- List.nub <$> getValue key
          mapM (get moves) moveNames

    quickMoves <- getMoves "quickMoves"
    chargeMoves <- getMoves "cinematicMoves"

    let parent = case getValue "parentPokemonId" of
          Right parent -> Just parent
          -- XXX This can swallow parse errors?
          Left _ -> Nothing

    baseCaptureRate <- do
      encounter <- getValue "encounter"
      getObjectValueWithDefault encounter "baseCaptureRate" 0

    return $ PokemonBase.new species ptypes attack defense stamina
       evolutions quickMoves chargeMoves parent baseCaptureRate)
  (\ex -> Epic.fail $ ex ++ " in " ++ show pokemonSettings)

makeWeatherAffinity :: Epic.MonadCatch m => StringMap Type -> ItemTemplate -> m [Type]
makeWeatherAffinity types weatherAffinity = do
  ptypes <- getObjectValue weatherAffinity "pokemonType"
  mapM (get types) ptypes

getNameFromKey :: Epic.MonadCatch m => String -> ItemTemplate -> m String
getNameFromKey nameKey itemTemplate =
  getObjectValue itemTemplate nameKey

makeObjects :: Epic.MonadCatch m => String -> (ItemTemplate -> m String) -> (ItemTemplate -> m a) -> [ItemTemplate]
  -> m (StringMap a)
makeObjects filterKey getName makeObject itemTemplates =
  foldr (\ itemTemplate maybeHash -> do
      hash <- maybeHash
      name <- getName itemTemplate
      obj <- makeObject itemTemplate
      return $ HashMap.insert name obj hash)
    (pure HashMap.empty)
    $ getAll itemTemplates filterKey

getAll :: [ItemTemplate] -> String -> [ItemTemplate]
getAll itemTemplates filterKey =
  mapMaybe (\ itemTemplate ->
    case getObjectValue itemTemplate filterKey of
      Right value -> Just value
      _ -> Nothing)
    itemTemplates

getFirst :: Epic.MonadCatch m => [ItemTemplate] -> String -> m ItemTemplate
getFirst itemTemplates filterKey =
  case getAll itemTemplates filterKey of
    [head] -> return head
    _ -> Epic.fail $ "Expected exactly one " ++ show filterKey

-- This seems roundabout, but the good thing is that the type "a" is
-- inferred from the usage context so the result is error-checked.
--
getObjectValue :: (Epic.MonadCatch m, Yaml.FromJSON a) => Yaml.Object -> String -> m a
getObjectValue yamlObject key =
  toEpic $ Yaml.parseEither (.: convertText key) yamlObject

getObjectValueWithDefault :: (Epic.MonadCatch m, Yaml.FromJSON a) => Yaml.Object -> String -> a -> m a
getObjectValueWithDefault yamlObject key dflt =
  toEpic $ Yaml.parseEither (\p -> p .:? convertText key .!= dflt) yamlObject

get :: Epic.MonadCatch m => StringMap a -> String -> m a
get map key =
  case HashMap.lookup key map of
    Just value -> return value
    _ -> Epic.fail $ "Key not found: " ++ show key

getType :: Epic.MonadCatch m => GameMaster -> String -> m Type
getType this typeName =
  get (GameMaster.types this) ("POKEMON_TYPE_" ++ (map toUpper typeName))

getWeatherBonus :: GameMaster -> Maybe Weather -> Type -> Float
getWeatherBonus this maybeWeather =
  case maybeWeather of
    Just weather ->
      case HashMap.lookup weather (weatherBonusMap this) of
        Just map -> (\ptype -> HashMap.lookupDefault 1 ptype map)
        Nothing -> error $ "No weatherBonusMap for " ++ show weather
    Nothing -> const 1

toWeather :: String -> Weather
toWeather string = case string of
  "CLEAR" -> Clear
  "FOG" -> Fog
  "OVERCAST" -> Overcast
  "PARTLY_CLOUDY" -> PartlyCloudy
  "RAINY" -> Rainy
  "SNOW" -> Snow
  "WINDY" -> Windy
  _ -> error $ "Unknown weather: " ++ string

toEpic :: (Show a, Epic.MonadCatch m) => Either a b -> m b
toEpic either =
  case either of
    Left err -> Epic.fail $ show err
    Right val -> return val

isSpecies :: GameMaster -> String -> Bool
isSpecies this speciesName =
  HashMap.member (sanitize speciesName) (GameMaster.pokemonBases this)

costAtLevel :: GameMaster -> (GameMaster -> [(Int, Float)]) -> Float -> Maybe Int
costAtLevel this func level =
  case nextLevel this level of
    Nothing -> Nothing
    Just _ ->
      case List.find ((== level) . snd) $ func this of
        Just (cost, _) -> Just cost
        Nothing -> Nothing

dustAtLevel :: GameMaster -> Float -> Maybe Int
dustAtLevel this =
  costAtLevel this GameMaster.dustAndLevel

candyAtLevel :: GameMaster -> Float -> Maybe Int
candyAtLevel this =
  costAtLevel this GameMaster.candyAndLevel
