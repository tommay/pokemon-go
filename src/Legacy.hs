-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module Legacy (
  load,
) where

import qualified Epic
import           StringMap (StringMap)
import qualified Util

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

import qualified Exception

import qualified Debug as D

-- Returns a map from species String to ([fast move name], [charged move name])
--
load :: Epic.MonadCatch m => FilePath -> IO (m (StringMap ([String], [String])))
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Left yamlParseException -> Epic.fail $ "Error in " ++ filename ++ ":\n" ++
      (Exception.displayException yamlParseException)
    Right yamlObjects -> return $ makeLegacyMap yamlObjects

makeLegacyMap :: Epic.MonadCatch m =>
  [Yaml.Object] -> m (StringMap ([String], [String]))
makeLegacyMap yamlObjects = do
  HashMap.map (List.partition ("_FAST" `List.isSuffixOf`)) <$>
    foldr (\ yamlObject mStringMap -> do
        stringMap <- mStringMap
        species <-
          Epic.toEpic $ Yaml.parseEither (.: "species") yamlObject
        moves <-
          Epic.toEpic $ Yaml.parseEither (.: "moves") yamlObject
        moves <- return $ map Util.toUpper moves
        return $ HashMap.insertWith (++) species moves stringMap)
      (pure HashMap.empty)
      yamlObjects
