-- So .: works with Strings.
{-# LANGUAGE OverloadedStrings #-}

module MyPokemon where

import qualified Data.Yaml as Yaml
import Data.Yaml (FromJSON(..), (.:))  -- ???

import qualified Epic
import qualified Stats
import           Stats (Stats)

data MyPokemon = MyPokemon {
  name        :: String,
  species     :: String,
  quickName   :: String,
  chargeName  :: String,
  cp          :: Integer,
  hp          :: Integer,
  stats       :: Maybe [Stats]
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

load :: Epic.MonadCatch m => FilePath -> IO (m [MyPokemon])
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Right myPokemon -> return $ pure myPokemon
    Left yamlParseException -> Epic.fail $ show yamlParseException

attack :: Epic.MonadCatch m => MyPokemon -> m Integer
attack this = do
  stats <- getStats this
  return $ Stats.attack stats

defense :: Epic.MonadCatch m => MyPokemon -> m Integer
defense this = do
  stats <- getStats this
  return $ Stats.defense stats

stamina :: Epic.MonadCatch m => MyPokemon -> m Integer
stamina this = do
  stats <- getStats this
  return $ Stats.stamina stats

getStats :: Epic.MonadCatch m => MyPokemon -> m Stats
getStats this =
  case MyPokemon.stats this of
    Just (stats:_) -> return stats
    Nothing -> Epic.fail $ "No stats for " ++ (MyPokemon.name this)
