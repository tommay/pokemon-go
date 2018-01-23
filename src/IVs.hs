-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module IVs (
  IVs (IVs),
  new,
  level,
  attack,
  defense,
  stamina,
  getAll,
  setLevel,
) where

import qualified Yaml

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:))
import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

data IVs = IVs {
  level       :: Float,
  attack      :: Int,
  defense     :: Int,
  stamina     :: Int
} deriving (Show)

instance Yaml.FromJSON IVs where
  parseJSON = Yaml.withObject "IVs" $ \y ->
    IVs <$>
    y .: "level" <*>
    y .: "attack" <*>
    y .: "defense" <*>
    y .: "stamina"

instance Builder.ToYaml IVs where
  toYaml this =
    Builder.mapping [
      "level" .= level this,
      "attack" .= attack this,
      "defense" .= defense this,
      "stamina" .= stamina this
    ]

new = IVs

getAll :: IVs -> (Float, Int, Int, Int)
getAll this =
  (level this, attack this, defense this, stamina this)

setLevel :: IVs -> Float -> IVs
setLevel this level =
  this { level = level }
