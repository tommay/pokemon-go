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

data Move = Move deriving (Show)

type Result = Maybe [Yaml.Object]

load :: FilePath -> IO (Either ParseException Result)
load filename =
  fmap createGameMaster <$> Yaml.decodeFileEither filename

createGameMaster :: Yaml.Object -> Result
createGameMaster yamlObject =
  

  case getItemTemplates yamlObject of
    Just itemTemplates ->
      let types = filter (has "typeEffective") itemTemplates
      in Just types
    _ -> Nothing

has :: Text -> Yaml.Object -> Bool
has = HashMap.member

getItemTemplates :: Yaml.Object -> Maybe [Yaml.Object]
getItemTemplates yamlObject =
  Yaml.parseMaybe (.: "itemTemplates") yamlObject

{-
  GameMaster HashMap.empty HashMap.empty Vector.empty Vector.empty
-}
