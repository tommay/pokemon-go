-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module Legacy (
  load,
) where

import qualified Epic
import           StringMap (StringMap)

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:))

import qualified Data.HashMap.Strict as HashMap

import qualified Debug as D

load :: Epic.MonadCatch m => FilePath -> IO (m (StringMap [String]))
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Left yamlParseException -> Epic.fail $ show yamlParseException
    Right yamlObjects -> return $ makeLegacyMap yamlObjects

makeLegacyMap :: Epic.MonadCatch m => [Yaml.Object] -> m (StringMap [String])
makeLegacyMap yamlObjects = do
  foldr (\ yamlObject mStringMap -> do
      stringMap <- mStringMap
      species <-
        toEpic $ Yaml.parseEither (.: "species") yamlObject
      moves <-
        toEpic $ Yaml.parseEither (.: "moves") yamlObject
      return $ HashMap.insert species moves stringMap)
    (pure HashMap.empty)
    yamlObjects

toEpic :: (Show a, Epic.MonadCatch m) => Either a b -> m b
toEpic either =
  case either of
    Left err -> Epic.fail $ show err
    Right val -> return val
