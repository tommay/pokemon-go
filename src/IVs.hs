-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module IVs (
  IVs (IVs),
  new,
  defaultIVs,
  IVs.null,
  level,
  attack,
  defense,
  stamina,
  getAll,
  setLevel,
  tweakLevel,
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

defaultIVs :: IVs
defaultIVs = new 30 13 13 13

null :: IVs
null = new 0 0 0 0

getAll :: IVs -> (Float, Int, Int, Int)
getAll this =
  (level this, attack this, defense this, stamina this)

setLevel :: IVs -> Float -> IVs
setLevel this level =
  this { level = level }

tweakLevel :: (Float -> Float) -> IVs -> IVs
tweakLevel func this =
  IVs.setLevel this $ func $ IVs.level this

