{-# LANGUAGE OverloadedStrings #-}

module Mythical (
  MythicalMap,
  load,
  isLegendary,
  isMythical,
) where

import qualified Epic

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.Yaml as Yaml

data Status = Ordinary | Legendary | Mythical
  deriving (Eq, Show)

instance Yaml.FromJSON Status where
  parseJSON (Yaml.String "legendary") = pure Legendary
  parseJSON (Yaml.String "mythical") = pure Mythical
  parseJSON Yaml.Null = pure Ordinary
  parseJSON oops = fail $
    "Expected pokemon to be legendary, mythical, or empty, got " ++ show oops

type MythicalMap = HashMap String Status

load :: Epic.MonadCatch m => FilePath -> IO (m MythicalMap)
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Right mythicalMap -> return $ pure mythicalMap
    Left yamlParseException ->
      Epic.fail $ Yaml.prettyPrintParseException yamlParseException

isMythical :: MythicalMap -> String -> Bool
isMythical = is Mythical

isLegendary :: MythicalMap -> String -> Bool
isLegendary = is Legendary

is :: Status -> MythicalMap -> String -> Bool
is status mythicalMap species =
  case HashMap.lookup (map Char.toLower species) mythicalMap of
    Just status' -> status == status'
    _ -> False
