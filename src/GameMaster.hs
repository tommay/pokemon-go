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
  pokemonBases  :: HashMap String PokemonBase,
  moves         :: HashMap String Move,
  cpMultipliers :: Vector Float,
  stardustCost  :: Vector Int
} deriving (Show)

data PokemonBase = PokemonBase {
  types        :: [String],
  attack       :: Int,
  defense      :: Int,
  stamina      :: Int,
  evolutions   :: [String],
  quickMoves   :: [String],
  chargeMoves  :: [String],
  parent       :: Maybe String
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

type Result = Maybe [Yaml.Object]

load :: FilePath -> IO (Either ParseException Result)
load filename =
  fmap makeGameMaster <$> Yaml.decodeFileEither filename

makeGameMaster :: Yaml.Object -> Result
makeGameMaster yamlObject = do
  itemTemplates <- getItemTemplates yamlObject
  types <- getTypes itemTemplates
  moves <- getMoves types itemTemplates
  return []

getTypes :: [ItemTemplate] -> Maybe (TextMap Type)
getTypes itemTemplates = do
  battleSettings <- getFirst "battleSettings" itemTemplates
  stab <- getFloatMaybe "sameTypeAttackBonusMultiplier" battleSettings
  makeObjects "typeEffective" "attackType" (makeType stab) itemTemplates

getMoves :: TextMap Type -> [ItemTemplate] -> Maybe (TextMap Move)
getMoves types itemTemplates =
  makeObjects "moveSettings" "movementId" (makeMove types) itemTemplates

getAll :: Text -> [ItemTemplate] -> [ItemTemplate]
getAll filterKey itemTemplates =
  filter (hasKey filterKey) itemTemplates

getFirst :: Text -> [ItemTemplate] -> Maybe ItemTemplate
getFirst filterKey itemTemplates =
  case getAll filterKey itemTemplates of
    [head] -> Just head
    _ -> Nothing

getFloatMaybe :: Text -> ItemTemplate -> Maybe Float
getFloatMaybe key itemTemplate =
  Yaml.parseMaybe (.: key) itemTemplate

makeObjects :: Text -> Text -> (ItemTemplate -> Maybe a) -> [ItemTemplate]
  -> Maybe (TextMap a)
makeObjects filterKey nameKey makeObject itemTemplates =
  Just $ foldr (\ yamlObject hash ->
    case HashMap.lookup nameKey yamlObject of
      Just (Yaml.String name) ->
        case makeObject yamlObject of
          Just obj ->
            HashMap.insert name obj hash
          _ -> hash
      _ -> hash)
    HashMap.empty
    $ getAll filterKey itemTemplates

makeType :: Float -> ItemTemplate -> Maybe Type
makeType stab itemTemplate =
  Just $ Type HashMap.empty "" stab

get :: TextMap a -> Text -> Maybe a
get map key = HashMap.lookup key map

makeMove :: TextMap Type -> ItemTemplate -> Maybe Move
makeMove types itemTemplate = do
  typeName <- Yaml.parseMaybe (.: "type") itemTemplate
  moveType <- get types typeName
  return $ Move moveType 0 0 0

{-
  Move <$>
    moveType <*>
    HashMap.lookup itemTemlate "power"
-}

{-
 <*>
    y .: "power" <*>
    y .: "duration" <*>
    y .: "energy"
-}

-- "hasKey" can be done the Yaml.Parser way but it's really convoluted
-- compared to this simple key lookup.
--
hasKey :: Text -> Yaml.Object -> Bool
hasKey = HashMap.member

-- Here it's nice to use Yaml.Parser because it will error if we don't
-- get a [ItemTemplate], i.e., it checks that the Yaml.Values are the
-- correct type thanks to type inference.
--
getItemTemplates :: Yaml.Object -> Maybe [ItemTemplate]
getItemTemplates yamlObject =
  Yaml.parseMaybe (.: "itemTemplates") yamlObject

{-
  GameMaster HashMap.empty HashMap.empty Vector.empty Vector.empty
-}
