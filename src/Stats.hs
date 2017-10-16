-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module Stats (
  Stats (Stats),
  level,
  attack,
  defense,
  stamina,
) where

import qualified Data.Yaml as Yaml
import Data.Yaml (FromJSON(..), (.:))  -- ???

data Stats = Stats {
  level       :: Float,
  attack      :: Integer,
  defense     :: Integer,
  stamina     :: Integer
} deriving (Show)

instance Yaml.FromJSON Stats where
  parseJSON (Yaml.Object y) =
    Stats <$>
    y .: "level" <*>
    y .: "attack" <*>
    y .: "defense" <*>
    y .: "stamina"
  parseJSON _ = fail "Expected Yaml.Object for Stats.parseJSON"
