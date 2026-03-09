-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module Legacy (
  LegacyMoveList,
  load,
  species,
  fastMoveNames,
  chargedMoveNames
) where

import qualified Epic
import qualified Util

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:))
import qualified Data.List as List
import qualified Exception

import qualified Debug as D

data LegacyMoveList = LegacyMoveList {
  species   :: String,
  fastMoveNames :: [String],
  chargedMoveNames :: [String]
} deriving (Show)

load :: Epic.MonadCatch m => FilePath -> IO (m [LegacyMoveList])
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Left yamlParseException -> Epic.fail $ "Error in " ++ filename ++ ":\n" ++
      (Exception.displayException yamlParseException)
    Right yamlObjects -> return $ mapM makeLegacyMoveList yamlObjects

makeLegacyMoveList :: Epic.MonadCatch m => Yaml.Object -> m LegacyMoveList
makeLegacyMoveList yamlObject = do
  species <- Epic.toEpic $ Yaml.parseEither (.: "species") yamlObject
  moveNames <- Epic.toEpic $ Yaml.parseEither (.: "moves") yamlObject
  let (fastMoveNames, chargedMoveNames) =
        List.partition ("_FAST" `List.isSuffixOf`) $ map Util.toUpper moveNames
  return $ LegacyMoveList {
    species = species,
    fastMoveNames = fastMoveNames,
    chargedMoveNames = chargedMoveNames
  }
