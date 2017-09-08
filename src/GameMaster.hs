-- So .: works with Strings.
{-# LANGUAGE OverloadedStrings #-}

module GameMaster where

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Yaml as Yaml
import           Data.Yaml (ParseException)
import Data.Yaml (FromJSON(..), (.:))  -- ???
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)

data GameMaster = GameMaster {
  pokemonBases  :: TextMap PokemonBase,
  moves         :: TextMap Move,
  cpMultipliers :: Vector Float,
  stardustCost  :: Vector Int
} deriving (Show)

data PokemonBase = PokemonBase {
  types        :: [Type],
  attack       :: Int,
  defense      :: Int,
  stamina      :: Int,
  evolutions   :: [Text],
  quickMoves   :: [Move],
  chargeMoves  :: [Move],
  parent       :: Maybe Text
} deriving (Show)

data Type = Type {
  effectiveness :: TextMap Float,
  name          :: Text,
  stab          :: Float
} deriving (Show)

data Move = Move {
  moveType :: Type,
  power    :: Float,
  duration :: Float,
  energy   :: Float
} deriving (Show)

type ItemTemplate = Yaml.Object

type TextMap = HashMap Text

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
  stab <- case getObjectValue battleSettings "sameTypeAttackBonusMultiplier" of
    Right stab -> stab
    Left error -> Left $ error ++ " in " ++ show battleSettings
  makeObjects "typeEffective" "attackType" (makeType stab) itemTemplates

-- XXX this is not done yet.
makeType :: Float -> ItemTemplate -> MaybeFail Type
makeType stab itemTemplate =
  Right $ Type HashMap.empty "" stab

getMoves :: TextMap Type -> [ItemTemplate] -> MaybeFail (TextMap Move)
getMoves types itemTemplates =
  makeObjects "moveSettings" "movementId" (makeMove types) itemTemplates

makeMove :: TextMap Type -> ItemTemplate -> MaybeFail Move
makeMove types itemTemplate = do
  let getTemplateValue text = getObjectValue itemTemplate text
  Move
    <$> do
      typeName <- getTemplateValue "type"
      get types typeName
    <*> getTemplateValue "power"
    <*> getTemplateValue "duration"
    <*> getTemplateValue "energy"

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

  return $ PokemonBase ptypes attack defense stamina evolutions
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
  concat $ map (\ itemTemplate ->
    case getObjectValue itemTemplate filterKey of
      Right value -> [value]
      _ -> [])
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

get :: TextMap a -> Text -> MaybeFail a
get map key =
  case HashMap.lookup key map of
    Just value -> Right value
    _ -> Left $ "Key not found: " ++ show key
