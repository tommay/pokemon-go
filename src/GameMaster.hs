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
  getAllTypes,
  getWeatherBonus,
  defaultWeatherBonus,
  dustAndLevel,
  candyAndLevel,
  allLevels,
  nextLevel,
) where

import qualified Epic
import qualified Legacy
import qualified Move
import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PvpChargedMove
import           PvpChargedMove (PvpChargedMove)
import qualified PvpFastMove
import           PvpFastMove (PvpFastMove)
import           StringMap (StringMap)
import qualified Type
import           Type (Type)
import qualified Util
import qualified Weather
import           Weather (Weather (..))
import           WeatherBonus (WeatherBonus)

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:), (.:?), (.!=))

import           Control.Monad (join)
import           Data.Text.Conversions (convertText)
import qualified Data.List as List
import qualified Data.List.Extra
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
  pvpFastMoves  :: StringMap PvpFastMove,
  pvpChargedMoves :: StringMap PvpChargedMove,
  forms         :: StringMap [String],
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
    Right yamlObject -> do
      gameMaster <- makeGameMaster yamlObject
      legacyMap <- join $ Legacy.load "legacy_moves.yaml"
      gameMaster <- addLegacyMoves legacyMap gameMaster
      return $ return $ gameMaster

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
  let removeLegacy = filter (not . Move.isLegacy)
      battleStats base =
        (PokemonBase.pokemonId base,
         PokemonBase.types base,
         PokemonBase.attack base,
         PokemonBase.defense base,
         PokemonBase.stamina base,
         removeLegacy $ PokemonBase.quickMoves base,
         removeLegacy $ PokemonBase.chargeMoves base)
  in Data.List.Extra.nubOn battleStats $ List.sortOn PokemonBase.species $
       HashMap.elems $ pokemonBases this

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
             Util.toLower (commaSeparated forms) ++ "."

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

-- allLevels returns all levels up through 45 which is the max level
-- with buddy cp boost.
--
allLevels :: GameMaster -> [Float]
allLevels =
  init . concat . map (\ (level, _) -> [level, level + 0.5])
    . zip [1..] . Vector.toList . cpMultipliers

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
  in Util.toUpper $ Regex.subRegex nonWordChars string "_"

makeGameMaster :: Epic.MonadCatch m => Yaml.Object -> m GameMaster
makeGameMaster yamlObject = do
  itemTemplates <- getItemTemplates yamlObject
  types <- getTypes itemTemplates
  moves <- getMoves types itemTemplates
  pvpFastMoves <- getPvpFastMoves types itemTemplates
  pvpChargedMoves <- getPvpChargedMoves types itemTemplates
  forms <- getForms itemTemplates
  pokemonBases <-
    makeObjects "pokemonSettings" (getSpeciesForPokemonBase forms)
      (makePokemonBase types moves pvpFastMoves pvpChargedMoves forms)
      (filter (hasFormIfRequired forms) itemTemplates)
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
  return $ GameMaster.new types moves pvpFastMoves pvpChargedMoves
    forms pokemonBases cpMultipliers stardustCost candyCost weatherBonusMap

-- Here it's nice to use Yaml.Parser because it will error if we don't
-- get a [ItemTemplate], i.e., it checks that the Yaml.Values are the
-- correct type thanks to type inference.
--
getItemTemplates :: Epic.MonadCatch m => Yaml.Object -> m [ItemTemplate]
getItemTemplates yamlObject =
  Epic.toEpic $ Yaml.parseEither (.: "itemTemplates") yamlObject

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
    <*> pure False

isFastMove :: ItemTemplate -> Bool
isFastMove itemTemplate = case getObjectValue itemTemplate "uniqueId" of
    Left _ -> error $ "Move doesn' have a uniqueId: " ++ show itemTemplate
    Right uniqueId -> List.isSuffixOf "_FAST" uniqueId

isChargedMove :: ItemTemplate -> Bool
isChargedMove = not . isFastMove

getPvpFastMoves :: Epic.MonadCatch m =>
  StringMap Type -> [ItemTemplate] -> m (StringMap PvpFastMove)
getPvpFastMoves types itemTemplates =
  getPvpMoves isFastMove (makePvpFastMove types) itemTemplates

getPvpChargedMoves :: Epic.MonadCatch m =>
  StringMap Type -> [ItemTemplate] -> m (StringMap PvpChargedMove)
getPvpChargedMoves types itemTemplates =
  getPvpMoves isChargedMove (makePvpChargedMove types) itemTemplates

getPvpMoves :: Epic.MonadCatch m =>
  (ItemTemplate -> Bool) -> (ItemTemplate -> m a) -> [ItemTemplate]
  -> m (StringMap a)
getPvpMoves pred makePvpMove itemTemplates =
  makeSomeObjects pred "combatMove" (getNameFromKey "uniqueId")
    makePvpMove itemTemplates

makePvpFastMove :: Epic.MonadCatch m =>
  StringMap Type -> ItemTemplate -> m PvpFastMove
makePvpFastMove types itemTemplate =
  let getTemplateValue = getObjectValue itemTemplate
      getTemplateValueWithDefault text =
        getObjectValueWithDefault itemTemplate text
  in PvpFastMove.new
    <$> getTemplateValue "uniqueId"
    <*> do
      typeName <- getTemplateValue "type"
      get types typeName
    <*> getTemplateValueWithDefault "power" 0.0
    <*> getTemplateValueWithDefault "durationTurns" 0
    <*> getTemplateValueWithDefault "energyDelta" 0
    <*> pure False

makePvpChargedMove :: Epic.MonadCatch m =>
  StringMap Type -> ItemTemplate -> m PvpChargedMove
makePvpChargedMove types itemTemplate =
  let getTemplateValue = getObjectValue itemTemplate
      getTemplateValueWithDefault text =
        getObjectValueWithDefault itemTemplate text
  in PvpChargedMove.new
    <$> getTemplateValue "uniqueId"
    <*> do
      typeName <- getTemplateValue "type"
      get types typeName
    <*> getTemplateValueWithDefault "power" 0.0
    <*> getTemplateValueWithDefault "energyDelta" 0
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
    Right forms -> mapM (\ form -> getObjectValue form "form") forms

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
  StringMap Type -> StringMap Move
  -> StringMap PvpFastMove -> StringMap PvpChargedMove
  -> StringMap [String] -> ItemTemplate -> m PokemonBase
makePokemonBase types moves pvpFastMoves pvpChargedMoves
    forms pokemonSettings =
  Epic.catch (do
    let getValue key = getObjectValue pokemonSettings key

    pokemonId <- getValue "pokemonId"

    species <- do
      species <- getSpeciesForPokemonBase forms pokemonSettings
      let normal = Regex.mkRegex "_normal$"
      return $ Regex.subRegex normal (Util.toLower species) ""

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
            evolution <- case getObjectValue branch "form" of
              Right form -> return form
              Left _ -> getObjectValue branch "evolution"
            candyCost <- case getObjectValue branch "candyCost" of
              Right candyCost -> return candyCost
              Left _ -> getValue "candyToEvolve"
            return (evolution, candyCost))
            -- Parts of GAME_MASTER seem to be in shambles as gen 4 is
            -- being released.  Filter out seemingly malformed elements
            -- that don't have an evolution.
            $ filter (HashMap.member "evolution") evolutionBranch
        -- XXX This can swallow parse errors?
        Left _ -> return []

    -- XXX Smeargle's doesn't have keys for "quickMoves" and
    -- "cinematicMoves".  Instead, those keys are in the template
    -- "SMEARGLE_MOVE_SETTINGS".  Smeargle is the only pokemon like
    -- that.  Since I will never care about smeargle's moves, they are
    -- just returned as [] here.  If they start using this scheme for
    -- other pokemon they will break and I can decide what to do then.
    --
    let getMoves key stringMap = if species /= "smeargle"
          then do
            -- Mew has some moves specified multiple times so nub them.
            moveNames <- List.nub <$> getValue key
            mapM (get stringMap) moveNames
          else return []

    quickMoves <- getMoves "quickMoves" moves
    chargeMoves <- getMoves "cinematicMoves" moves

    pvpFastMoves <- getMoves "quickMoves" pvpFastMoves
    pvpChargedMoves <- getMoves "cinematicMoves" pvpChargedMoves

    let parent = case getValue "parentPokemonId" of
          Right parent -> Just parent
          -- XXX This can swallow parse errors?
          Left _ -> Nothing

    baseCaptureRate <- do
      encounter <- getValue "encounter"
      getObjectValueWithDefault encounter "baseCaptureRate" 0

    thirdMoveCost <- do
      thirdMove <- getValue "thirdMove"
      -- Smeargle has candyToUnlock but not stardustToUnlock so default it.
      stardustToUnlock <-
        getObjectValueWithDefault thirdMove "stardustToUnlock" 0
      candyToUnlock <-  getObjectValue thirdMove "candyToUnlock"
      return $ (stardustToUnlock, candyToUnlock)

    purificationCost <- do
      case getValue "shadow" of
        Right shadow -> do
          purificationStardustNeeded <-
            getObjectValue shadow "purificationStardustNeeded"
          purificationCandyNeeded <-
            getObjectValue shadow "purificationCandyNeeded"
          return $ (purificationStardustNeeded, purificationCandyNeeded)
        Left _ -> return $ (0, 0)

    return $ PokemonBase.new pokemonId species ptypes attack defense stamina
       evolutions quickMoves chargeMoves pvpFastMoves pvpChargedMoves
       parent baseCaptureRate thirdMoveCost purificationCost
    )
  (\ex -> Epic.fail $ ex ++ " in " ++ show pokemonSettings)

makeWeatherAffinity :: Epic.MonadCatch m => StringMap Type -> ItemTemplate -> m [Type]
makeWeatherAffinity types weatherAffinity = do
  ptypes <- getObjectValue weatherAffinity "pokemonType"
  mapM (get types) ptypes

getNameFromKey :: Epic.MonadCatch m => String -> ItemTemplate -> m String
getNameFromKey nameKey itemTemplate =
  getObjectValue itemTemplate nameKey

makeObjects :: Epic.MonadCatch m =>
  String -> (ItemTemplate -> m String) -> (ItemTemplate -> m a)
  -> [ItemTemplate]
  -> m (StringMap a)
makeObjects = makeSomeObjects $ const True

makeSomeObjects :: Epic.MonadCatch m =>
  (ItemTemplate -> Bool)
  -> String -> (ItemTemplate -> m String) -> (ItemTemplate -> m a)
  -> [ItemTemplate]
  -> m (StringMap a)
makeSomeObjects pred filterKey getName makeObject itemTemplates =
  foldr (\ itemTemplate maybeHash -> do
      hash <- maybeHash
      name <- getName itemTemplate
      obj <- makeObject itemTemplate
      return $ HashMap.insert name obj hash)
    (pure HashMap.empty)
    $ filter pred
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
  Epic.toEpic $ Yaml.parseEither (.: convertText key) yamlObject

getObjectValueWithDefault :: (Epic.MonadCatch m, Yaml.FromJSON a) => Yaml.Object -> String -> a -> m a
getObjectValueWithDefault yamlObject key dflt =
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

commaSeparated :: [String] -> String
commaSeparated [] = ""
commaSeparated [a] = a
commaSeparated [a, b] = a ++ " or " ++ b
commaSeparated list =
  let commaSeparated' [a, b] = a ++ ", or " ++ b
      commaSeparated' (h:t) = h ++ ", " ++ commaSeparated' t
  in commaSeparated' list

addLegacyMoves :: Epic.MonadCatch m =>
  StringMap [String] -> GameMaster -> m GameMaster
addLegacyMoves legacyMap this =
  let addMoves :: Epic.MonadCatch m =>
        String -> [String] -> m GameMaster -> m GameMaster
      addMoves species moveNames mGameMaster = do
        gameMaster <- mGameMaster
        moves <- mapM (getMove gameMaster) moveNames
        moves <- return $ map Move.setLegacy moves
        base <- getPokemonBase gameMaster species
        base <- return $ foldr PokemonBase.addMove base moves
        let speciesU = Util.toUpper species
            key = if HashMap.member speciesU $ pokemonBases gameMaster
              then speciesU
              else speciesU ++ "_NORMAL"
        return $ gameMaster {
          pokemonBases = HashMap.insert key base $ pokemonBases gameMaster
        }
  in HashMap.foldrWithKey addMoves (pure this) legacyMap
