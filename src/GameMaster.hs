-- So .: works with Strings.
{-# LANGUAGE OverloadedStrings #-}

module GameMaster where

import           Data.Hashable (Hashable)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Yaml as Yaml
import           Data.Yaml (ParseException)
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)

import           TextMap (TextMap)
import qualified Type
import           Type (Type)
import qualified Move
import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)

data GameMaster = GameMaster {
  pokemonBases  :: TextMap PokemonBase,
  moves         :: TextMap Move,
  cpMultipliers :: Vector Float,
  stardustCost  :: Vector Int
} deriving (Show)

type ItemTemplate = Yaml.Object

type MaybeFail = Either String

load :: FilePath -> IO (MaybeFail GameMaster)
load filename = do
  either <- Yaml.decodeFileEither filename
  return $ case either of
    Left yamlParseException -> Left $ show yamlParseException
    Right yamlObject -> makeGameMaster yamlObject

makeGameMaster :: Yaml.Object -> MaybeFail GameMaster
makeGameMaster yamlObject = do
  itemTemplates <- getItemTemplates yamlObject
  types <- getTypes itemTemplates
  moves <- getMoves types itemTemplates
  let pokemonBases =
        makeObjects "pokemonSettings" "pokemonId"
          (makePokemonBase types moves) itemTemplates
  cpMultipliers <- do
    playerLevel <- getFirst itemTemplates "playerLevel"
    getObjectValue playerLevel "cpMultiplier"
  stardustCost <- do
    pokemonUpgrades <- getFirst itemTemplates "pokemonUpgrades"
    getObjectValue pokemonUpgrades "stardustCost"
  return $ GameMaster HashMap.empty moves cpMultipliers stardustCost

-- Here it's nice to use Yaml.Parser because it will error if we don't
-- get a [ItemTemplate], i.e., it checks that the Yaml.Values are the
-- correct type thanks to type inference.
--
getItemTemplates :: Yaml.Object -> MaybeFail [ItemTemplate]
getItemTemplates yamlObject =
  Yaml.parseEither (.: "itemTemplates") yamlObject

getTypes :: [ItemTemplate] -> MaybeFail (TextMap Type)
getTypes itemTemplates = do
  battleSettings <- getFirst itemTemplates "battleSettings"
  stab <- getObjectValue battleSettings "sameTypeAttackBonusMultiplier"
  makeObjects "typeEffective" "attackType" (makeType stab) itemTemplates

makeType :: Float -> ItemTemplate -> MaybeFail Type
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

effectivenessOrder :: [Text]
effectivenessOrder =
  map (\ ptype -> Text.append "POKEMON_TYPE_" $ Text.toUpper ptype)
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

getMoves :: TextMap Type -> [ItemTemplate] -> MaybeFail (TextMap Move)
getMoves types itemTemplates =
  makeObjects "moveSettings" "movementId" (makeMove types) itemTemplates

makeMove :: TextMap Type -> ItemTemplate -> MaybeFail Move
makeMove types itemTemplate = do
  let getTemplateValue text = getObjectValue itemTemplate text
  Move.Move
    <$> do
      typeName <- getTemplateValue "pokemonType"
      get types typeName
    <*> getObjectValueWithDefault itemTemplate "power" 0
    <*> ((/1000) <$> getTemplateValue "durationMs")
    <*> getObjectValueWithDefault itemTemplate "energyDelta" 0

makePokemonBase :: TextMap Type -> TextMap Move -> ItemTemplate -> MaybeFail PokemonBase
makePokemonBase types moves pokemonSettings = do
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
  defense <- getObjectValue statsObject "defense"
  stamina <- getObjectValue statsObject "stamina"

  evolutions <- do
    case getValue "evolutionBranch" of
      Right evolutionBranch ->
        mapM (\ branch -> getObjectValue branch "evolution") evolutionBranch
      -- XXX This can swallow parse errors?
      Left _ -> Right []

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
    quickMoves chargeMoves parent

makeObjects :: Text -> Text -> (ItemTemplate -> MaybeFail a) -> [ItemTemplate]
  -> MaybeFail (TextMap a)
makeObjects filterKey nameKey makeObject itemTemplates =
  foldr (\ itemTemplate maybeHash -> case maybeHash of
      Right hash -> do
        name <- getObjectValue itemTemplate nameKey
        obj <- makeObject itemTemplate
        return $ HashMap.insert name obj hash
      hash -> hash)
    (Right HashMap.empty)
    $ getAll itemTemplates filterKey

getAll :: [ItemTemplate] -> Text -> [ItemTemplate]
getAll itemTemplates filterKey =
  mapMaybe (\ itemTemplate ->
    case getObjectValue itemTemplate filterKey of
      Right value -> Just value
      _ -> Nothing)
    itemTemplates

getFirst :: [ItemTemplate] -> Text -> MaybeFail ItemTemplate
getFirst itemTemplates filterKey =
  case getAll itemTemplates filterKey of
    [head] -> Right head
    _ -> Left $ "Expected exactly one " ++ show filterKey

-- This seems roundabout, but the good thing is that the type "a" is
-- inferred from the usage context so the result is error-checked.
--
getObjectValue :: FromJSON a => Yaml.Object -> Text -> MaybeFail a
getObjectValue yamlObject key =
  Yaml.parseEither (.: key) yamlObject

getObjectValueWithDefault :: FromJSON a => Yaml.Object -> Text -> a -> MaybeFail a
getObjectValueWithDefault yamlObject key dflt =
  Yaml.parseEither (\p -> p .:? key .!= dflt) yamlObject

get :: TextMap a -> Text -> MaybeFail a
get map key =
  case HashMap.lookup key map of
    Just value -> Right value
    _ -> Left $ "Key not found: " ++ show key
