-- So .: works with Strings.
{-# LANGUAGE OverloadedStrings #-}

module MyPokemon where

import qualified Data.Yaml as Yaml
import Data.Yaml (FromJSON(..), (.:))  -- ???

import qualified Epic

data MyPokemon = MyPokemon {
  name        :: String,
  species     :: String,
  quick       :: String,
  charge      :: String,
  cp          :: Integer,
  hp          :: Integer,
  stats       :: Maybe [Stat]
} deriving (Show)

instance Yaml.FromJSON MyPokemon where
  parseJSON (Yaml.Object y) =
    MyPokemon <$>
    y .: "name" <*>
    y .: "species" <*>
    y .: "quick" <*>
    y .: "charge" <*>
    y .: "cp" <*>
    y .: "hp" <*>
    y .: "stats"
  parseJSON _ = fail "Expected Yaml.Object for MyPokemon.parseJSON"

data Stat = Stat {
  level       :: Float,
  attack      :: Int,
  defense     :: Int,
  stamins     :: Int
} deriving (Show)

instance Yaml.FromJSON Stat where
  parseJSON (Yaml.Object y) =
    Stat <$>
    y .: "level" <*>
    y .: "attack" <*>
    y .: "defense" <*>
    y .: "stamina"
  parseJSON _ = fail "Expected Yaml.Object for Stats.parseJSON"

load :: Epic.MonadCatch m => FilePath -> IO (m [MyPokemon])
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Right myPokemon -> return $ pure myPokemon
    Left yamlParseException -> Epic.fail $ show yamlParseException
