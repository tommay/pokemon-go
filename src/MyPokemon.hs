-- So .: works with Strings.
{-# LANGUAGE OverloadedStrings #-}

module MyPokemon where

import qualified Data.Yaml as Yaml
import Data.Yaml (FromJSON(..), (.:))  -- ???

data Pokemon = Pokemon {
  name        :: String,
  species     :: String,
  cp          :: Int,
  stats       :: Maybe [Stat]
} deriving (Show)

instance Yaml.FromJSON Pokemon where
  parseJSON (Yaml.Object y) =
    Pokemon <$>
    y .: "name" <*>
    y .: "species" <*>
    y .: "cp" <*>
    y .: "stats"
  parseJSON _ = fail "Expected Yaml.Object for Pokemon.parseJSON"

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

load :: FilePath -> IO (Either Yaml.ParseException [Pokemon])
load filename =
  Yaml.decodeFileEither filename
