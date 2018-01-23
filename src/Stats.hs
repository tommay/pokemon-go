-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module Stats (
  Stats (Stats),
  new,
  attack,
  defense,
  stamina,
) where

import qualified Yaml

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:))
import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

data Stats = Stats {
  attack      :: Float,
  defense     :: Float,
  stamina     :: Int
} deriving (Show)

instance Yaml.FromJSON Stats where
  parseJSON = Yaml.withObject "Stats" $ \y ->
    Stats <$>
    y .: "attack" <*>
    y .: "defense" <*>
    y .: "stamina"

instance Builder.ToYaml Stats where
  toYaml this =
    Builder.mapping [
      "attack" .= attack this,
      "defense" .= defense this,
      "stamina" .= stamina this
    ]

new = Stats
