-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module Stats (
  Stats (Stats),
  new,
  level,
  attack,
  defense,
  stamina,
  getAll,
) where

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:))
import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

import qualified Data.Scientific as Scientific
import           Data.Semigroup ((<>))

data Stats = Stats {
  level       :: Float,
  attack      :: Int,
  defense     :: Int,
  stamina     :: Int
} deriving (Show)

instance Yaml.FromJSON Stats where
  parseJSON = Yaml.withObject "Stats" $ \y ->
    Stats <$>
    y .: "level" <*>
    y .: "attack" <*>
    y .: "defense" <*>
    y .: "stamina"

instance Builder.ToYaml Stats where
  toYaml this =
    Builder.mapping [
      "level" .= level this,
      "attack" .= attack this,
      "defense" .= defense this,
      "stamina" .= stamina this
    ]

instance Builder.ToYaml Float where
  toYaml = Builder.scientific . Scientific.fromFloatDigits

new = Stats

getAll :: Stats -> (Float, Int, Int, Int)
getAll this =
  (level this, attack this, defense this, stamina this)
