-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

-- This uses the Data.Store serialization library to cache a binary
-- version of GameMaster because reading GAME_MASTER.yaml has gotten
-- pretty slow as the file gets larger and larger.  To do this,
-- GameMaster and the types it uses need to be instances of the
-- library's serializable typeclass.  The easiest way to do that is to
-- use GHC.Generics and make the types instances of Generic and then
-- they can be made instances of the appropriate type with some kind
-- of Generic magic
--
-- Note that the cache file is about three times bigger than the yaml
-- file because all the Types and Moves get written for every
-- PokemonBase, and all the string are ucs-32 or something instead of
-- utf-8.
--
-- Reading it back in seems to take half the time of reading the yaml
-- file which isn't as good as I'd hoped.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameMaster (
  GameMaster,
  load,
  getPokemonBase,
  getQuick,
  getCharge,
  getCpMultiplier,
  allLevelAndCost,
  getLevelsForStardust,
  getStardustForLevel,
  allPokemonBases,
  allSpecies,
  getType,
  getAllTypes,
  getWeatherBonus,
  defaultWeatherBonus,
  dustAndLevel,
  candyAndLevel,
  allLevels,
  powerUpLevels,
  nextLevel,
  shadowStardustMultiplier,
  shadowCandyMultiplier,
  purifiedStardustMultiplier,
  purifiedCandyMultiplier,
  luckyPowerUpStardustDiscountPercent,
) where

import qualified Cost
import           Cost (Cost)
import qualified Epic
import qualified Legacy
import qualified Move
import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokemonClass (PokemonClass (..))
import           PokemonClass (PokemonClass)
import qualified PvpChargedMove
import           PvpChargedMove (PvpChargedMove)
import qualified PvpFastMove
import           PvpFastMove (PvpFastMove)
import           StringMap (StringMap)
import qualified TempEvoOverride
import           TempEvoOverride (TempEvoOverride (TempEvoOverride))
import qualified Type
import           Type (Type)
import qualified Util
import qualified Weather
import           Weather (Weather (..))
import           WeatherBonus (WeatherBonus)

import qualified Data.ByteString as B
import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:), (.:?), (.!=))
import qualified Data.Store as Store
import           GHC.Generics (Generic)

import           Control.Applicative ((<|>))
import           Control.Applicative.HT (lift2)
import           Control.Monad (join, liftM, liftM2, filterM)
import           Control.Monad.Extra (anyM)
import qualified Data.Either as Either
import           Data.Text.Conversions (convertText)
import qualified Data.List as List
import qualified Data.List.Extra
import qualified Data.Maybe as Maybe
import           Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vector
import           Data.Vector (Vector, (!))
import qualified System.Directory
import qualified System.IO.Error
import qualified Text.Regex as Regex

import qualified Debug as D

data GameMaster = GameMaster {
  types         :: StringMap Type,
  moves         :: StringMap Move,
  forms         :: StringMap [String],
  pokemonBases  :: StringMap PokemonBase,
  cpMultipliers :: Vector Float,
  stardustCost  :: [Int],
  candyCost     :: [Int],
  shadowStardustMultiplier   :: Float,
  shadowCandyMultiplier      :: Float,
  purifiedStardustMultiplier :: Float,
  purifiedCandyMultiplier    :: Float,
  xlCandyCost   :: [Int],
  luckyPowerUpStardustDiscountPercent :: Float,
  weatherBonusMap :: HashMap Weather (HashMap Type Float)
} deriving (Show, Generic)

instance Store.Store GameMaster
instance Store.Store Type
instance Store.Store Move
instance Store.Store PokemonBase
instance Store.Store Weather
instance Store.Store PokemonClass
instance Store.Store TempEvoOverride

type ItemTemplate = Yaml.Object

new = GameMaster

-- GAME_MASTER.yaml was updated so that sometimes moves' uniqueId is a
-- String and sometimes a number.  Here, I atttempt to roll with it.
-- XXX I'm not sure this is the most aesthetic way.  Maybe just having
-- asString convert a Yaml.Value to a String would be better.  I tried
-- identifying moves with a Yaml.Value but it was a pain trrying to
-- make it an instance of Store.Store.

newtype StringOrNumber = S { asString :: String }

instance Yaml.FromJSON StringOrNumber where
  parseJSON v =
    let fromNumber = Yaml.withScientific "fromNumber" $ pure . S . show
        fromText = Yaml.withText "fromText" $ pure . S . convertText
    in fromNumber v <|> fromText v <|>
         fail ("expected String or Number, got " ++ show v)

load :: Epic.MonadCatch m => IO (m GameMaster)
load = do
  let filename = "GAME_MASTER.yaml"
      legacyMovesFilename = "legacy_moves.yaml"
      cacheFileName = "/tmp/" ++ filename ++ ".cache"
  maybeCachedGameMaster <- do
    anyNewer <- anyM (`isFileNewerThan` cacheFileName)
      [filename, legacyMovesFilename]
    if not anyNewer
      then maybeLoadFromCache cacheFileName
      else return $ Nothing
  case maybeCachedGameMaster of
    Just gameMaster -> return $ pure gameMaster
    Nothing -> do
      mGameMaster <- loadFromYaml filename
      case mGameMaster of
         Left exception -> Epic.fail $ show exception
         Right gameMaster -> do
           writeCache cacheFileName gameMaster
           return $ pure gameMaster

isFileNewerThan :: FilePath -> FilePath -> IO Bool
a `isFileNewerThan` b = do
  -- If both are Just then return a > b else True, i.e., if either file
  -- is missing consider the cache to be out of date.
  let maybeGT :: Ord a => Maybe a -> Maybe a -> Bool
      maybeGT a b = Maybe.fromMaybe True $ lift2 (>) a b
  liftM2 maybeGT (getMaybeModificationTime a) (getMaybeModificationTime b)

-- Returns a file's modification time if the file exists and we can get
-- its modification time, or Nothing.
--
getMaybeModificationTime :: FilePath -> IO (Maybe UTCTime)
getMaybeModificationTime filename = do
  either <- System.IO.Error.tryIOError $
    System.Directory.getModificationTime filename
  return $ case either of
    Left ioError -> Nothing
    Right time -> Just time

loadFromYaml :: Epic.MonadCatch m => FilePath -> IO (m GameMaster)
loadFromYaml filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Left yamlParseException -> Epic.fail $ show yamlParseException
    Right yamlObjects -> do
      -- Have to load Legacy here while we're in the IO monad.
      legacyMap <- join $ Legacy.load "legacy_moves.yaml"
      return $ makeGameMaster yamlObjects legacyMap

writeCache :: FilePath -> GameMaster -> IO ()
writeCache filename gameMaster =
  B.writeFile filename $ Store.encode gameMaster  

maybeLoadFromCache :: FilePath -> IO (Maybe GameMaster)
maybeLoadFromCache filename = do
  either <- System.IO.Error.tryIOError $ B.readFile filename
  return $ case either of
    Left ioError -> Nothing
    Right byteString -> case Store.decode byteString of
      Left peekException -> Nothing
      Right gameMaster -> Just gameMaster

allPokemonBases :: GameMaster -> [PokemonBase]
allPokemonBases this =
  -- Doing nubOn battleStats filters out pokemon that, for battle, are
  -- identical to pokemon earlier in the list, such as shadow and
  -- purified pokemon which are effectively the same as non-shadow and
  -- non-purified pokemon, and also weird things like the _FALL_2019
  -- pokemon wearing halloween costumes.  It should robustly filter
  -- out any further weird things down the line.  Sorting by the
  -- species name first will put the plain-named pokemon first so
  -- they will be kept by nub.
  -- XXX Currently (11/2019) dugtrio_purified has 2 more defense than normal
  -- so dugtrio_purified makes it through.
  let battleStats base =
        (PokemonBase.pokemonId base,
         PokemonBase.types base,
         PokemonBase.attack base,
         PokemonBase.defense base,
         PokemonBase.stamina base,
         PokemonBase.quickMoves base,
         PokemonBase.chargeMoves base)
  in Data.List.Extra.nubOn battleStats $ List.sortOn PokemonBase.species $
       HashMap.elems $ pokemonBases this

allSpecies :: GameMaster -> [String]
allSpecies = map PokemonBase.species . GameMaster.allPokemonBases

-- See the comment in makeGameMaster about Unown.
--
getPokemonBase :: Epic.MonadCatch m => GameMaster -> String -> m PokemonBase
getPokemonBase this speciesName =
  let getPokemonBase' = GameMaster.lookup "species" (pokemonBases this)
  in case getPokemonBase' speciesName of
       Right pokemonBase -> return pokemonBase
       Left _ -> case getPokemonBase' (speciesName ++ "_NORMAL") of
         Right pokemonBase -> return pokemonBase
         Left _ -> do
           forms <- GameMaster.lookup "species" (forms this) speciesName
           Epic.fail $
             speciesName ++ " has no normal form.  Specify one of " ++
             Util.toLower (Util.commaSeparated "or" forms) ++ "."

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

-- Each cost is used both for the level and for a half level up.
-- x2 duplicates each item in a list.
--
x2 :: [a] -> [a]
x2 [] = []
x2 (a:as) = a : a : x2 as

levelsFrom :: Float -> [Float]
levelsFrom first =
  let levels = first : map (+0.5) levels
  in levels

levels :: [Float]
levels = levelsFrom 1

allLevelAndCost :: GameMaster -> [(Float, Cost)]
allLevelAndCost this =
  -- candyCost has the candy cost for levels 1 - 39 then zeros for the
  -- XL levels.  xlCandyCost has values for only the XL levels.
  -- Create a complete list of xlCandy for all levels by replacing the
  -- non-zero costs in candyCost with zero until they become zero,
  -- then use the xlCandyCosts.
  let makeXlCandy (0:_) xlCandys = xlCandys
      makeXlCandy (_:candys) xlCandys = 0 : makeXlCandy candys xlCandys
      candy = candyCost this
      xlCandy = makeXlCandy candy $ xlCandyCost this
      toCost (dust, candy, xlCandy) = Cost.new dust candy xlCandy
      allCosts = map toCost $ zip3 (stardustCost this) candy xlCandy
  in zip levels $ x2 allCosts

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

-- allLevels returns all levels up through 45 which is the max level
-- that team leaders and Team Rocket have.  Normal players can power
-- up to 40 then get one more level with best buddy oost.
--
allLevels :: GameMaster -> [Float]
allLevels =
  map fst . zip levels . init . x2 . Vector.toList . cpMultipliers

-- powerUpLevels returns the levels that pokemon can be powered up to.
--
powerUpLevels :: GameMaster -> [Float]
powerUpLevels this =
  let powerupFromLevels = map fst $ allLevelAndCost this
  in case powerupFromLevels of
       [] -> []
       (head:_) -> head : map (+ 0.5) powerupFromLevels

nextLevel :: GameMaster -> Float -> Maybe (Int, Int, Float)
nextLevel this level =
  let candy = Maybe.fromJust $ candyAtLevel this level
      stardust = Maybe.fromJust $ dustAtLevel this level
  in case filter (> level) $ allLevels this of
       [] -> Nothing
       list -> Just (candy, stardust, minimum list)

getLevelsForStardust :: (Epic.MonadCatch m) => GameMaster -> Int -> m [Float]
getLevelsForStardust this starDust =
  case map snd $ filter ((== starDust) . fst) $ dustAndLevel this of
    [] -> Epic.fail $ "Bad dust amount: " ++ show starDust
    levels -> return levels

getStardustForLevel :: GameMaster -> Float -> Int
getStardustForLevel this level =
  fst $ head $ filter ((== level) . snd) $ dustAndLevel this

lookup :: Epic.MonadCatch m => String -> StringMap a -> String -> m a
lookup what hash key =
  case HashMap.lookup (sanitize key) hash of
    Just val -> return val
    Nothing -> Epic.fail $ "No such " ++ what ++ ": " ++ key

sanitize :: String -> String
sanitize string =
  let nonWordChars = Regex.mkRegex "\\W"
  in Util.toUpper $ Regex.subRegex nonWordChars string "_"

makeGameMaster :: Epic.MonadCatch m =>
  [Yaml.Object] -> StringMap ([String], [String]) -> m GameMaster
makeGameMaster yamlObjects legacyMap = do
  itemTemplates <- getItemTemplates yamlObjects
  types <- getTypes itemTemplates
  pvpFastMoves <- getPvpFastMoves types itemTemplates
  pvpChargedMoves <- getPvpChargedMoves types itemTemplates
  moves <- getMoves (makeMaybeMove types pvpFastMoves pvpChargedMoves)
    itemTemplates
  forms <- getForms itemTemplates
  pokemonBases <-
    makeObjects "pokemonSettings" (getSpeciesForPokemonBase forms)
      (makePokemonBase types moves forms legacyMap)
      -- Unown doesn't have a form, so it gets filtered here.  That's
      -- ok because Unown isn't a meta pokemon so it can be ignored.
      -- But it's kind of awkward when "./counter unown" fails with
      -- "unown has no normal form.  Specify one of unown_f, unown_a,
      -- ..." and "./counter unown_f" fails with "No such species:
      -- unown_f".
      (filter (hasFormIfRequired forms) itemTemplates)
  pokemonBases <- return $ insertTempEvos pokemonBases
  cpMultipliers <- do
    playerLevel <- getFirst itemTemplates "playerLevel"
    getObjectValue playerLevel "cpMultiplier"
  let getFromPokemonUpgrades name = do
        pokemonUpgrades <- getFirst itemTemplates "pokemonUpgrades"
        getObjectValue pokemonUpgrades name
  stardustCost <- getFromPokemonUpgrades "stardustCost"
  candyCost <- getFromPokemonUpgrades "candyCost"
  shadowStardustMultiplier <- getFromPokemonUpgrades "shadowStardustMultiplier"
  shadowCandyMultiplier <- getFromPokemonUpgrades "shadowCandyMultiplier"
  purifiedStardustMultiplier <- getFromPokemonUpgrades "purifiedStardustMultiplier"
  purifiedCandyMultiplier <- getFromPokemonUpgrades "purifiedCandyMultiplier"
  xlCandyCost <- getFromPokemonUpgrades "xlCandyCost"
  luckyPowerUpStardustDiscountPercent <- do
     luckyPokemonSetttings <- getFirst itemTemplates "luckyPokemonSettings"
     getObjectValue luckyPokemonSetttings "powerUpStardustDiscountPercent"
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
  return $ GameMaster.new types moves forms pokemonBases cpMultipliers
    stardustCost candyCost
    shadowStardustMultiplier shadowCandyMultiplier
    purifiedStardustMultiplier purifiedCandyMultiplier
    xlCandyCost
    luckyPowerUpStardustDiscountPercent
    weatherBonusMap

insertTempEvos :: StringMap PokemonBase -> StringMap PokemonBase
insertTempEvos basesMap =
  List.foldr insertPokemonBases basesMap $
    map PokemonBase.makeTempEvos $ HashMap.elems basesMap

insertPokemonBases :: [PokemonBase] -> StringMap PokemonBase ->
  StringMap PokemonBase
insertPokemonBases pokemonBases basesMap =
  List.foldr insertPokemonBase basesMap pokemonBases

insertPokemonBase :: PokemonBase -> StringMap PokemonBase ->
  StringMap PokemonBase
insertPokemonBase pokemonBase basesMap =
   HashMap.insert (Util.toUpper $ PokemonBase.species pokemonBase) pokemonBase basesMap

-- The yaml file is one big array of "templates":
-- - templateId: AR_TELEMETRY_SETTINGS
--   data:
--     templateId: AR_TELEMETRY_SETTINGS
-- What we really want is the "data" hash from each "template" element.
-- So get the template array then map over it to extract the data.
--
-- Here it's nice to use Yaml.Parser because it will error if we don't
-- get a [ItemTemplate], i.e., it checks that the Yaml.Values are the
-- correct type thanks to type inference.
--
getItemTemplates :: Epic.MonadCatch m => [Yaml.Object] -> m [ItemTemplate]
getItemTemplates yamlObjects =
  Epic.toEpic $ mapM (Yaml.parseEither (.: "data")) yamlObjects

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
  map (\ ptype -> "POKEMON_TYPE_" ++ Util.toUpper ptype)
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

-- This is tricky.  Some of the PVE moves don't have a corresponding PVP
-- move.  In that case makeMaybeMove will return (m Nothing) for the
-- itemTemplate instead of (m Just Move), then the Nothings will be removed
-- from the StringMap.
--
getMoves :: Epic.MonadCatch m =>
  (ItemTemplate -> m (Maybe Move)) -> [ItemTemplate] ->
  m (StringMap Move)
getMoves makeMaybeMove itemTemplates = do
  maybeMoveMap <- makeObjects "moveSettings"
      (liftM asString . getNameFromKey "movementId")
    makeMaybeMove itemTemplates
  -- Use (mapMaybe id) to discard any Nothing values, and turn Just Move
  -- into Move values.
  return $ HashMap.mapMaybe id maybeMoveMap

makeMaybeMove :: Epic.MonadCatch m =>
  StringMap Type -> StringMap PvpFastMove ->
  StringMap PvpChargedMove -> ItemTemplate -> m (Maybe Move)
makeMaybeMove types pvpFastMoves pvpChargedMoves itemTemplate = do
  let getTemplateValue text = getObjectValue itemTemplate text
      getTemplateValueWithDefault text dflt =
        getObjectValueWithDefault dflt itemTemplate text
  movementId <- asString <$> getTemplateValue "movementId"
  vfxName <- getTemplateValue "vfxName"
  let maybeMoveStats = case HashMap.lookup movementId pvpFastMoves of
        Just pvpFastMove -> Just (PvpFastMove.power pvpFastMove,
          PvpFastMove.energyDelta pvpFastMove,
          PvpFastMove.durationTurns pvpFastMove)
        Nothing -> case HashMap.lookup movementId pvpChargedMoves of
          -- Charged moves used to get a durationTurns of 'error "Charged
          -- moves have no durationTurns"' which was clever but broke
          -- the GAME_MASTER cache because it had to be evaluated to write
          -- it to the cache and it just threw the error.  So just use 0.
          Just pvpChargedMove -> Just (PvpChargedMove.power pvpChargedMove,
            PvpChargedMove.energyDelta pvpChargedMove,
            0)
          Nothing -> Nothing
  case maybeMoveStats of
    Nothing -> return Nothing
    Just (pvpPower, pvpEnergyDelta, pvpDurationTurns) -> do
      Just <$> (Move.new
        <$> pure movementId
        <*> pure vfxName
        <*> do
          typeName <- getTemplateValue "pokemonType"
          get types typeName
        <*> getObjectValueWithDefault 0 itemTemplate "power"
        <*> ((/1000) <$> getTemplateValue "durationMs")
        -- The move "return" (at least) no longer has damageWindowStartMs,
        -- so assume 0, at least for now.
        <*> getTemplateValueWithDefault "damageWindowStartMs" 0
        <*> getObjectValueWithDefault 0 itemTemplate "energyDelta"
        <*> pure pvpPower
        <*> pure pvpEnergyDelta
        <*> pure pvpDurationTurns
        <*> pure False)

-- TRANSFORM_FAST is the only move without an energyDelta, so default it to 0
-- and consider anything >= 0 to be a fast move.
--
isFastMove :: Epic.MonadCatch m => ItemTemplate -> m Bool
isFastMove itemTemplate = do
  energyDelta <-
     getObjectValueWithDefault (0 :: Integer) itemTemplate "energyDelta"
  return $ energyDelta >= 0

isChargedMove :: Epic.MonadCatch m => ItemTemplate -> m Bool
isChargedMove = liftM not . isFastMove

getPvpFastMoves :: Epic.MonadCatch m =>
  StringMap Type -> [ItemTemplate] -> m (StringMap PvpFastMove)
getPvpFastMoves types itemTemplates =
  getPvpMoves isFastMove (makePvpFastMove types) itemTemplates

getPvpChargedMoves :: Epic.MonadCatch m =>
  StringMap Type -> [ItemTemplate] -> m (StringMap PvpChargedMove)
getPvpChargedMoves types itemTemplates =
  getPvpMoves isChargedMove (makePvpChargedMove types) itemTemplates

getPvpMoves :: Epic.MonadCatch m =>
  (ItemTemplate -> m Bool) -> (ItemTemplate -> m a) -> [ItemTemplate]
  -> m (StringMap a)
getPvpMoves pred makePvpMove itemTemplates =
  makeSomeObjects pred "combatMove"
     (liftM asString . getNameFromKey "uniqueId")
    makePvpMove itemTemplates

makePvpFastMove :: Epic.MonadCatch m =>
  StringMap Type -> ItemTemplate -> m PvpFastMove
makePvpFastMove types itemTemplate =
  let getTemplateValue = getObjectValue itemTemplate
      getTemplateValueWithDefault dflt text =
        getObjectValueWithDefault dflt itemTemplate text
  in PvpFastMove.new
    <$> (asString <$> getTemplateValue "uniqueId")
    <*> do
      typeName <- asString <$> getTemplateValue "type"
      get types typeName
    <*> getTemplateValueWithDefault 0.0 "power"
    <*> getTemplateValueWithDefault 0 "durationTurns"
    <*> getTemplateValueWithDefault 0 "energyDelta"
    <*> pure False

makePvpChargedMove :: Epic.MonadCatch m =>
  StringMap Type -> ItemTemplate -> m PvpChargedMove
makePvpChargedMove types itemTemplate =
  let getTemplateValue = getObjectValue itemTemplate
      getTemplateValueWithDefault dflt text =
        getObjectValueWithDefault dflt itemTemplate text
  in PvpChargedMove.new
    <$> (asString <$> getTemplateValue "uniqueId")
    <*> do
      typeName <- asString <$> getTemplateValue "type"
      get types typeName
    <*> getTemplateValueWithDefault 0.0 "power"
    <*> getTemplateValueWithDefault 0 "energyDelta"
    <*> pure False

-- Return a map of pokemon species to a (possibly empty) list of its forms.
--
getForms :: Epic.MonadCatch m => [ItemTemplate] -> m (StringMap [String])
getForms itemTemplates =
  makeObjects "formSettings" (getNameFromKey "pokemon")
    getFormNames itemTemplates

getFormNames :: Epic.MonadCatch m => ItemTemplate -> m [String]
getFormNames formSettings =
  case getObjectValue formSettings "forms" of
    Left _ -> return []
    Right forms -> return $
      Either.rights $ map (\ form -> getObjectValue form "form") forms

-- This is a little goofy because GAME_MASTER is a little goofy.  There are
-- three templates for the two exeggutor forms.  The first and third are
-- redundant with each other:
--   pokemonId: EXEGGUTOR
-- and 
--   pokemonId: EXEGGUTOR
--   form: EXEGGUTOR_ALOLA
-- and
--   pokemonId: EXEGGUTOR
--   form: EXEGGUTOR_NORMAL
-- I want the original exeggutor to be known as just "exeggutor", and
-- the alola form to be known as "exeggutor_alola".  So, if there is a
-- form and the form does not end with "_NORMAL" then use it as the
-- species, else use the pokemonId.  This also works with DEOXYS.
-- Note that we'll add both redundant templates to the pokemonBases
-- hash, but the first one will be overwritten which is fine because
-- except for the form they are identical.
--
getSpeciesForPokemonBase :: Epic.MonadCatch m => StringMap [String] -> ItemTemplate -> m String
getSpeciesForPokemonBase forms itemTemplate = do
  species <- getObjectValue itemTemplate "pokemonId"
  case HashMap.lookup species forms of
    Nothing -> Epic.fail $ "Can't find forms for " ++ species
    Just [] -> return species
    Just _ -> case getObjectValue itemTemplate "form" of
      Right form -> return form
      Left _ -> Epic.fail $ "No form specified for " ++ species      

makePokemonBase :: Epic.MonadCatch m =>
  StringMap Type -> StringMap Move -> StringMap [String] ->
  StringMap ([String], [String]) -> ItemTemplate -> m PokemonBase
makePokemonBase types moves forms legacyMap pokemonSettings =
  Epic.catch (do
    let getValue key = getObjectValue pokemonSettings key
        getValueWithDefault dflt key =
          getObjectValueWithDefault dflt pokemonSettings key

    pokemonId <- getValue "pokemonId"

    species <- do
      species <-
        Util.toLower <$> getSpeciesForPokemonBase forms pokemonSettings
      let normal = Regex.mkRegex "_normal$"
      return $ Regex.subRegex normal species ""

    ptypes <- do
      ptype <- getValue "type"
      let ptypes = case getValue "type2" of
            Right ptype2 -> [ptype, ptype2]
            -- XXX This can swallow parse errors?
            Left _ -> [ptype]
      mapM (get types) ptypes
  
    statsObject <- getValue "stats"
    -- Sometimes new pokemon have an empty stats object for a while
    -- so just default to zero instead of choking.
    attack <- getObjectValueWithDefault 0 statsObject "baseAttack"
    defense <- getObjectValueWithDefault 0 statsObject "baseDefense"
    stamina <- getObjectValueWithDefault 0 statsObject "baseStamina"

    evolutions <- do
      case getValue "evolutionBranch" of
        Right evolutionBranch ->
          mapM (\ branch -> do
            evolution <- case getObjectValue branch "form" of
              Right form -> return form
              Left _ -> getObjectValue branch "evolution"
            candyCost <- case getObjectValue branch "candyCost" of
              Right candyCost -> return candyCost
              -- Gimmighoul doesn't evolve with candy so consider that to
              -- be zero candy.
              Left _ -> getValueWithDefault 0 "candyToEvolve"
            noCandyCostViaTrade <- getObjectValueWithDefault False
              branch "noCandyCostViaTrade"
            return (evolution, candyCost, noCandyCostViaTrade))
            -- Parts of GAME_MASTER seem to be in shambles as gen 4 is
            -- being released.  Filter out seemingly malformed elements
            -- that don't have an evolution.
            $ filter (HashMap.member "evolution") evolutionBranch
        -- XXX This can swallow parse errors?
        Left _ -> return []

    let (legacyFastMoveNames, legacyChargedMoveNames) =
          -- XXX This should be HashMap.findWithDefault which doesn't
          -- seem to exist.
          case HashMap.lookup species legacyMap of
            Just val -> val
            Nothing -> ([], [])

    -- legacyNames are the names from legacy_moves.yaml.  They get
    -- appended to the lists of elite moves in GAME_MASTER.yaml.
    --
    let getEliteMoves key legacyMoveNames = do
--        moveNames <- (liftM $ fmap asString) $ getValueWithDefault [] key
          moveNames <- (map asString) <$> getValueWithDefault [] key
          mapM (liftM Move.setLegacy . get moves) $
            moveNames ++ legacyMoveNames
    eliteQuickMoves <- getEliteMoves "eliteQuickMove" legacyFastMoveNames
    eliteChargeMoves <- getEliteMoves "eliteCinematicMove" legacyChargedMoveNames

    -- XXX Smeargle's doesn't have keys for "quickMoves" and
    -- "cinematicMoves".  Instead, those keys are in the template
    -- "SMEARGLE_MOVE_SETTINGS".  Smeargle is the only pokemon like
    -- that.  Since I will never care about smeargle's moves, they are
    -- just returned as [] here.  If they start using this scheme for
    -- other pokemon they will break and I can decide what to do then.
    --
    let getMoves key = if species /= "smeargle"
          then do
            moveNames <- (map asString) <$> getValue key
            return $ Maybe.catMaybes $
              map (flip HashMap.lookup $ moves) moveNames
          else return []

    quickMoves <- do
      moves <- getMoves "quickMoves"
      return $ List.nub $ moves ++ eliteQuickMoves
    chargeMoves <- do
      moves <- getMoves "cinematicMoves"
      return $ List.nub $ moves ++ eliteChargeMoves

    let parent = case getValue "parentPokemonId" of
          Right parent -> Just parent
          -- XXX This can swallow parse errors?
          Left _ -> Nothing

    baseCaptureRate <- do
      encounter <- getValue "encounter"
      getObjectValueWithDefault 0 encounter "baseCaptureRate"

    thirdMoveCost <- do
      thirdMove <- getValue "thirdMove"
      -- Smeargle has candyToUnlock but not stardustToUnlock so default it.
      stardustToUnlock <-
        getObjectValueWithDefault 0 thirdMove "stardustToUnlock"
      candyToUnlock <-  getObjectValue thirdMove "candyToUnlock"
      return $ (stardustToUnlock, candyToUnlock)

    let maybeShadow = rightToMaybe $ getValue "shadow"
        isShadowAvailable = Maybe.isJust maybeShadow
    purificationCost <-
      case maybeShadow of
        Just shadow -> do
          purificationStardustNeeded <-
            getObjectValue shadow "purificationStardustNeeded"
          purificationCandyNeeded <-
            getObjectValue shadow "purificationCandyNeeded"
          return $ (purificationStardustNeeded, purificationCandyNeeded)
        Nothing -> return $ (0, 0)

    pokemonClass <- getObjectValueWithDefault PokemonClass.Normal
      pokemonSettings "pokemonClass"

    tempEvoOverrides <- getTempEvoOverrides types pokemonSettings

    return $ PokemonBase.new pokemonId species ptypes attack defense stamina
       evolutions quickMoves chargeMoves parent baseCaptureRate
       thirdMoveCost purificationCost pokemonClass
       tempEvoOverrides False isShadowAvailable
    )
  (\ex -> Epic.fail $ ex ++ " in " ++ show pokemonSettings)

-- There is supposed to be Maybe.rightToMaybe.
-- XXX Using this swallows errors?
--
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

getTempEvoOverrides :: Epic.MonadCatch m =>
  StringMap Type -> ItemTemplate -> m [TempEvoOverride]
getTempEvoOverrides types pokemonSettings = do
  case getObjectValue pokemonSettings "tempEvoOverrides" of
    Right yamlObjects ->
      Maybe.catMaybes <$> mapM (getTempEvoOverride types) yamlObjects
    -- XXX This can swallow parse errors?
    Left _ -> return $ []

getTempEvoOverride :: Epic.MonadCatch m =>
  StringMap Type -> Yaml.Object -> m (Maybe TempEvoOverride)
getTempEvoOverride types yamlObject =
  case getObjectValue yamlObject "tempEvoId" of
    Right tempEvoId -> do
      ptypes <- do
        ptype <- getObjectValue yamlObject "typeOverride1"
        let ptypes = case getObjectValue yamlObject "typeOverride2" of
              Right ptype2 -> [ptype, ptype2]
              -- XXX This can swallow parse errors?
              Left _ -> [ptype]
        mapM (get types) ptypes
      statsObject <- getObjectValue yamlObject "stats"
      attack <- getObjectValue statsObject "baseAttack"
      defense <- getObjectValue statsObject "baseDefense"
      stamina <- getObjectValue statsObject "baseStamina"
      return $ Just TempEvoOverride {
        TempEvoOverride.tempEvoId = tempEvoId,
        TempEvoOverride.typeOverrides  = ptypes,
        TempEvoOverride.attack = attack,
        TempEvoOverride.defense = defense,
        TempEvoOverride.stamina = stamina
        }
    -- XXX This can swallow parse errors?
    Left _ -> return $ Nothing

instance Yaml.FromJSON PokemonClass where
  parseJSON (Yaml.String "POKEMON_CLASS_LEGENDARY") = pure PokemonClass.Legendary
  parseJSON (Yaml.String "POKEMON_CLASS_MYTHIC") = pure PokemonClass.Mythic
  parseJSON (Yaml.String "POKEMON_CLASS_ULTRA_BEAST") =
    pure PokemonClass.UltraBeast
  parseJSON oops = fail $
    "Expected pokemonClass to be legendary, mythic, or ultrabeast, got " ++
      show oops

makeWeatherAffinity :: Epic.MonadCatch m => StringMap Type -> ItemTemplate -> m [Type]
makeWeatherAffinity types weatherAffinity = do
  ptypes <- getObjectValue weatherAffinity "pokemonType"
  mapM (get types) ptypes

getNameFromKey :: (Epic.MonadCatch m, Yaml.FromJSON a) =>
  String -> ItemTemplate -> m a
getNameFromKey nameKey itemTemplate =
  getObjectValue itemTemplate nameKey

makeObjects :: Epic.MonadCatch m =>
  String -> (ItemTemplate -> m String) -> (ItemTemplate -> m a)
  -> [ItemTemplate]
  -> m (StringMap a)
makeObjects = makeSomeObjects $ const $ pure True

makeSomeObjects :: Epic.MonadCatch m =>
  (ItemTemplate -> m Bool)
  -> String -> (ItemTemplate -> m String) -> (ItemTemplate -> m a)
  -> [ItemTemplate]
  -> m (StringMap a)
makeSomeObjects pred filterKey getName makeObject itemTemplates = do
  filtered <- filterM pred $ getAll itemTemplates filterKey
  foldr (\ itemTemplate maybeHash -> do
      hash <- maybeHash
      name <- getName itemTemplate
      obj <- makeObject itemTemplate
      pure $ HashMap.insert name obj hash)
    (pure HashMap.empty)
    $ filtered

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

checkPokemonSettings :: ItemTemplate -> (Yaml.Object -> Bool) -> Bool
checkPokemonSettings itemTemplate predicate =
  case getObjectValue itemTemplate "pokemonSettings" of
    Left _ -> False
    Right pokemonSettings -> predicate pokemonSettings

hasFormIfRequired :: StringMap [String] -> ItemTemplate -> Bool
hasFormIfRequired forms itemTemplate =
  checkPokemonSettings itemTemplate $ \ pokemonSettings ->
    case getObjectValue pokemonSettings "pokemonId" of
      Left _ -> False
      Right species ->
        case HashMap.lookup species forms of
          Nothing -> False
          Just [] -> True
          _ -> case getObjectValue pokemonSettings "form" of
                 Left _ -> False
                 -- I would like to say Right _ -> True but the compiler
                 -- needs to know the type of _ so it can make/use the
                 -- correct variant of getObjectValue.  Rather than trying
                 -- to come up with a complex annotation for getObjectValue,
                 -- it is simpler to annotate form but ignore the value like
                 -- this:
                 Right form -> const True (form :: String)

-- This seems roundabout, but the good thing is that the type "a" is
-- inferred from the usage context so the result is error-checked.
--
getObjectValue :: (Epic.MonadCatch m, Yaml.FromJSON a) => Yaml.Object -> String -> m a
getObjectValue yamlObject key =
  Epic.toEpic $ case Yaml.parseEither (.: convertText key) yamlObject of
    Right val -> Right val
    Left error -> Left $ show yamlObject ++ ": " ++ error

getObjectValueWithDefault :: (Epic.MonadCatch m, Yaml.FromJSON a) => a -> Yaml.Object -> String -> m a
getObjectValueWithDefault dflt yamlObject key =
  Epic.toEpic $ Yaml.parseEither (\p -> p .:? convertText key .!= dflt) yamlObject

get :: Epic.MonadCatch m => StringMap a -> String -> m a
get map key =
  case HashMap.lookup key map of
    Just value -> return value
    _ -> Epic.fail $ "Key not found: " ++ show key

getType :: Epic.MonadCatch m => GameMaster -> String -> m Type
getType this typeName =
  get (GameMaster.types this) ("POKEMON_TYPE_" ++ (Util.toUpper typeName))

getAllTypes :: GameMaster -> [Type]
getAllTypes this =
  HashMap.elems $ types this

getWeatherBonus :: GameMaster -> Maybe Weather -> WeatherBonus
getWeatherBonus this maybeWeather =
  case maybeWeather of
    Just weather ->
      case HashMap.lookup weather (weatherBonusMap this) of
        Just map -> (\ptype -> HashMap.lookupDefault 1 ptype map)
        Nothing -> error $ "No weatherBonusMap for " ++ show weather
    Nothing -> defaultWeatherBonus

defaultWeatherBonus :: WeatherBonus
defaultWeatherBonus =
  const 1

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
