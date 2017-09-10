-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module GameMaster where

import           Data.Text.Conversions (convertText)
import           Data.Char (toUpper)
import           Data.Hashable (Hashable)
import           Data.Maybe (mapMaybe)
import qualified Data.Yaml as Yaml
import           Data.Yaml (ParseException)
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)

import qualified Epic
import           StringMap (StringMap)
import qualified Type
import           Type (Type)
import qualified Move
import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)

data GameMaster = GameMaster {
  pokemonBases  :: StringMap PokemonBase,
  moves         :: StringMap Move,
  cpMultipliers :: Vector Float,
  stardustCost  :: Vector Int
} deriving (Show)

type ItemTemplate = Yaml.Object

load :: Epic.MonadThrow m => FilePath -> IO (m GameMaster)
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Left yamlParseException -> Epic.fail $ show yamlParseException
    Right yamlObject -> return $ makeGameMaster yamlObject

makeGameMaster :: Epic.MonadThrow m => Yaml.Object -> m GameMaster
makeGameMaster yamlObject = do
  itemTemplates <- getItemTemplates yamlObject
  types <- getTypes itemTemplates
  moves <- getMoves types itemTemplates
  pokemonBases <-
    makeObjects "pokemonSettings" "pokemonId"
      (makePokemonBase types moves) itemTemplates
  cpMultipliers <- do
    playerLevel <- getFirst itemTemplates "playerLevel"
    getObjectValue playerLevel "cpMultiplier"
  stardustCost <- do
    pokemonUpgrades <- getFirst itemTemplates "pokemonUpgrades"
    getObjectValue pokemonUpgrades "stardustCost"
  return $ GameMaster pokemonBases moves cpMultipliers stardustCost

-- Here it's nice to use Yaml.Parser because it will error if we don't
-- get a [ItemTemplate], i.e., it checks that the Yaml.Values are the
-- correct type thanks to type inference.
--
getItemTemplates :: Epic.MonadThrow m => Yaml.Object -> m [ItemTemplate]
getItemTemplates yamlObject =
  toEpic $ Yaml.parseEither (.: "itemTemplates") yamlObject

getTypes :: Epic.MonadThrow m => [ItemTemplate] -> m (StringMap Type)
getTypes itemTemplates = do
  battleSettings <- getFirst itemTemplates "battleSettings"
  stab <- getObjectValue battleSettings "sameTypeAttackBonusMultiplier"
  makeObjects "typeEffective" "attackType" (makeType stab) itemTemplates

makeType :: Epic.MonadThrow m => Float -> ItemTemplate -> m Type
makeType stab itemTemplate = do
  attackScalar <- getObjectValue itemTemplate "attackScalar"
  let effectiveness = toMap $ zip effectivenessOrder attackScalar
  name <- getObjectValue itemTemplate "attackType"
  return $ Type.Type effectiveness name stab

-- XXX there must be a library function to so this.
--
toMap :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
toMap pairs =
  foldr (\ (k, v) hash -> HashMap.insert k v hash) HashMap.empty pairs

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

getMoves :: Epic.MonadThrow m => StringMap Type -> [ItemTemplate] -> m (StringMap Move)
getMoves types itemTemplates =
  makeObjects "moveSettings" "movementId" (makeMove types) itemTemplates

makeMove :: Epic.MonadThrow m => StringMap Type -> ItemTemplate -> m Move
makeMove types itemTemplate = do
  let getTemplateValue text = getObjectValue itemTemplate text
  Move.Move
    <$> do
      typeName <- getTemplateValue "pokemonType"
      get types typeName
    <*> getObjectValueWithDefault itemTemplate "power" 0
    <*> ((/1000) <$> getTemplateValue "durationMs")
    <*> getObjectValueWithDefault itemTemplate "energyDelta" 0

makePokemonBase :: Epic.MonadThrow m => StringMap Type -> StringMap Move -> ItemTemplate -> m PokemonBase
makePokemonBase types moves pokemonSettings =
  Epic.catch (do
    let getValue key = getObjectValue pokemonSettings key

    ptypes <- do
      ptype <- getValue "type"
      let ptypes = case getValue "type2" of
            Right ptype2 -> [ptype, ptype2]
            -- XXX This can swallow parse errors?
            Left _ -> [ptype]
      mapM (get types) ptypes
  
    statsObject <- getValue "stats"
    attack <- getObjectValue statsObject "attack"
    defense <- getObjectValue statsObject "baseDefense"
    stamina <- getObjectValue statsObject "baseStamina"

    evolutions <- do
      case getValue "evolutionBranch" of
        Right evolutionBranch ->
          mapM (\ branch -> getObjectValue branch "evolution") evolutionBranch
        -- XXX This can swallow parse errors?
        Left _ -> return []

    let getMoves key = do
          moveNames <- getValue key
          mapM (get moves) moveNames

    quickMoves <- getMoves "quickMoves"
    chargeMoves <- getMoves "cinematicMoves"

    let parent = case getValue "parentPokemonId" of
          Right parent -> Just parent
          -- XXX This can swallow parse errors?
          Left _ -> Nothing

    return $ PokemonBase.PokemonBase ptypes attack defense stamina evolutions
      quickMoves chargeMoves parent)
  (\ex -> Epic.fail $ "In " ++ (show pokemonSettings) ++ ": " ++ (show ex))

makeObjects :: Epic.MonadThrow m => String -> String -> (ItemTemplate -> m a) -> [ItemTemplate]
  -> m (StringMap a)
makeObjects filterKey nameKey makeObject itemTemplates =
  foldr (\ itemTemplate maybeHash -> do
      hash <- maybeHash
      name <- getObjectValue itemTemplate nameKey
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

getFirst :: Epic.MonadThrow m => [ItemTemplate] -> String -> m ItemTemplate
getFirst itemTemplates filterKey =
  case getAll itemTemplates filterKey of
    [head] -> return head
    _ -> Epic.fail $ "Expected exactly one " ++ show filterKey

-- This seems roundabout, but the good thing is that the type "a" is
-- inferred from the usage context so the result is error-checked.
--
getObjectValue :: Epic.MonadThrow m => FromJSON a => Yaml.Object -> String -> m a
getObjectValue yamlObject key =
  toEpic $ Yaml.parseEither (.: convertText key) yamlObject

getObjectValueWithDefault :: Epic.MonadThrow m => FromJSON a => Yaml.Object -> String -> a -> m a
getObjectValueWithDefault yamlObject key dflt =
  toEpic $ Yaml.parseEither (\p -> p .:? convertText key .!= dflt) yamlObject

get :: Epic.MonadThrow m => StringMap a -> String -> m a
get map key =
  case HashMap.lookup key map of
    Just value -> return value
    _ -> Epic.fail $ "Key not found: " ++ show key

toEpic :: (Show a, Epic.MonadThrow m) => Either a b -> m b
toEpic either =
  case either of
    Left err -> Epic.fail (show err)
    Right val -> return val
