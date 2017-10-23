-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Stats (
  Stats (Stats),
  level,
  attack,
  defense,
  stamina,
) where

import           GHC.Generics
import           Data.Semigroup ((<>))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Yaml as Yaml
import           Data.Yaml (FromJSON(..), (.:), (.=))

data Stats = Stats {
  level       :: Float,
  attack      :: Int,
  defense     :: Int,
  stamina     :: Int
} deriving (Show, Generic)

instance Yaml.FromJSON Stats where
  parseJSON (Yaml.Object y) =
    Stats <$>
    y .: "level" <*>
    y .: "attack" <*>
    y .: "defense" <*>
    y .: "stamina"
  parseJSON _ = fail "Expected Yaml.Object for Stats.parseJSON"

instance Yaml.ToJSON Stats where
  toEncoding this = Aeson.pairs $
    "level" .= level this <>
    "attack" .= attack this <>
    "defense" .= defense this <>
    "stamina" .= defense this
