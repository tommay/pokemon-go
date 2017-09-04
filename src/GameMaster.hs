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

load :: FilePath -> IO (Either ParseException (Maybe GameMaster))
load filename = do
  either <- Yaml.decodeFileEither filename
  return $ do
    yamlObject <- either
    return $ makeGameMaster yamlObject

makeGameMaster :: Yaml.Object -> Maybe GameMaster
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
getItemTemplates :: Yaml.Object -> Maybe [ItemTemplate]
getItemTemplates yamlObject =
  Yaml.parseMaybe (.: "itemTemplates") yamlObject

getTypes :: [ItemTemplate] -> Maybe (TextMap Type)
getTypes itemTemplates = do
  battleSettings <- getFirst itemTemplates "battleSettings"
  stab <- getObjectValue battleSettings "sameTypeAttackBonusMultiplier"
  makeObjects "typeEffective" "attackType" (makeType stab) itemTemplates

makeType :: Float -> ItemTemplate -> Maybe Type
makeType stab itemTemplate =
  Just $ Type HashMap.empty "" stab

getMoves :: TextMap Type -> [ItemTemplate] -> Maybe (TextMap Move)
getMoves types itemTemplates =
  makeObjects "moveSettings" "movementId" (makeMove types) itemTemplates

makeMove :: TextMap Type -> ItemTemplate -> Maybe Move
makeMove types itemTemplate = do
  let getTemplateValue text = getObjectValue itemTemplate text
  Move
    <$> do
      typeName <- getTemplateValue "type"
      get types typeName
    <*> getTemplateValue "power"
    <*> getTemplateValue "duration"
    <*> getTemplateValue "energy"

makePokemonBase :: TextMap Type -> TextMap Move -> ItemTemplate -> Maybe PokemonBase
makePokemonBase types moves pokemonSettings = do
  let getValue key = getObjectValue pokemonSettings key

  ptypes <- do
    ptype <- getValue "type"
    let ptypes = case getValue "type2" of
          Nothing -> [ptype]
          Just ptype2 -> [ptype, ptype2]
    mapM (get types) ptypes

  statsObject <- getValue "stats"
  attack <- getObjectValue statsObject "attack"
  defense <- getObjectValue statsObject "defense"
  stamina <- getObjectValue statsObject "stamina"

  evolutions <- do
    case getValue "evolutionBranch" of
      Just evolutionBranch ->
        mapM (\ branch -> getObjectValue branch "evolution") evolutionBranch
      Nothing -> Just []

  let getMoves key = do
        moveNames <- getValue key
        mapM (get moves) moveNames

  quickMoves <- getMoves "quickMoves"
  chargeMoves <- getMoves "cinematicMoves"

  let parent = getValue "parentPokemonId"

  return $ PokemonBase ptypes attack defense stamina evolutions
    quickMoves chargeMoves parent

makeObjects :: Text -> Text -> (ItemTemplate -> Maybe a) -> [ItemTemplate]
  -> Maybe (TextMap a)
makeObjects filterKey nameKey makeObject itemTemplates =
  foldr (\ itemTemplate maybeHash -> case maybeHash of
      Just hash -> do
        name <- getObjectValue itemTemplate nameKey
        obj <- makeObject itemTemplate
        return $ HashMap.insert name obj hash
      hash -> hash)
    (Just HashMap.empty)
    $ getAll itemTemplates filterKey

getAll :: [ItemTemplate] -> Text -> [ItemTemplate]
getAll itemTemplates filterKey =
  filter (hasKey filterKey) itemTemplates

getFirst :: [ItemTemplate] -> Text -> Maybe ItemTemplate
getFirst itemTemplates filterKey =
  case getAll itemTemplates filterKey of
    [head] -> Just head
    _ -> Nothing

-- This seems roundabout, but the good thing is that the type "a" is
-- inferred from the usage context so the result is error-checked.
--
getObjectValue :: FromJSON a => Yaml.Object -> Text -> Maybe a
getObjectValue yamlObject key =
  Yaml.parseMaybe (.: key) yamlObject

-- "hasKey" can be done the Yaml.Parser way but it's really convoluted
-- compared to this simple key lookup.
--
hasKey :: Text -> Yaml.Object -> Bool
hasKey = HashMap.member

get :: TextMap a -> Text -> Maybe a
get map key = HashMap.lookup key map
