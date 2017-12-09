{-# LANGUAGE OverloadedStrings #-}

module Matchup (
  Matchup,
  load,
  defender,
  attacker,
  quick,
  charge,
  dps,
  minDamage,
  maxDamage,
) where

import Epic

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:), (.:?))

data Matchup = Matchup {
  defender :: String,
  attacker :: String,
  quick :: String,
  charge :: String,
  dps :: Float,
  minDamage :: Int,
  maxDamage :: Int
} deriving (Show)

instance Yaml.FromJSON Matchup where
  parseJSON = Yaml.withObject "Matchup" $ \y ->
    Matchup <$>
    y .: "defender" <*>
    y .: "attacker" <*>
    y .: "quick" <*>
    y .: "charge" <*>
    y .: "dps" <*>
    y .: "minDamage" <*>
    y .: "maxDamage"

load :: Epic.MonadCatch m => FilePath -> IO (m [Matchup])
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Right results -> return $ pure results
    Left yamlParseException ->
      Epic.fail $ Yaml.prettyPrintParseException yamlParseException

