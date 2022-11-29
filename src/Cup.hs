-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module Cup (
  Cup,
  load,
  little,
  evolve,
  allowed,
  excluded,
  pokemon,
  banned,
  premier,
) where

import qualified Epic
import           StringMap (StringMap)

import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:), (.:?), (.!=))

import qualified Data.HashMap.Strict as HashMap
import           Data.Text.Conversions (convertText)

import qualified Exception

import qualified Debug as D

data Cup = Cup {
  name     :: String,
  little   :: Bool,
  evolve   :: Bool,
  allowed  :: [String],
  excluded :: [String],
  pokemon  :: [String],
  banned   :: [String],
  premier  :: Bool
}

-- Returns a map from species String to ([fast move name], [charged move name])
--
load :: Epic.MonadCatch m => FilePath -> IO (m (StringMap Cup))
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Left yamlParseException -> Epic.fail $ "Error in " ++ filename ++ ":\n" ++
      (Exception.displayException yamlParseException)
    Right yamlObjects -> return $ makeCups yamlObjects

-- This seems roundabout, but the good thing is that the type "a" is
-- inferred from the usage context so the result is error-checked.
--
getObjectValue :: (Epic.MonadCatch m, Yaml.FromJSON a) => Yaml.Object -> String -> m a
getObjectValue yamlObject key =
  Epic.toEpic $ Yaml.parseEither (.: convertText key) yamlObject

getObjectValueWithDefault :: (Epic.MonadCatch m, Yaml.FromJSON a) => a -> Yaml.Object -> String -> m a
getObjectValueWithDefault dflt yamlObject key =
  Epic.toEpic $ Yaml.parseEither (\p -> p .:? convertText key .!= dflt) yamlObject

makeCups :: Epic.MonadCatch m => [Yaml.Object] -> m (StringMap Cup)
makeCups yamlObjects =
  HashMap.fromList . map (\c -> (Cup.name c, c)) <$> mapM makeCup yamlObjects

makeCup :: Epic.MonadCatch m => Yaml.Object-> m Cup
makeCup yamlObject = do
  name <- getObjectValue yamlObject "name"
  little <- getObjectValueWithDefault False yamlObject "little"
  evolve <- getObjectValueWithDefault False yamlObject "evolve"
  allowed <- getObjectValueWithDefault [] yamlObject "allowed"
  excluded <- getObjectValueWithDefault [] yamlObject "excluded"
  pokemon <- getObjectValueWithDefault [] yamlObject "pokemon"
  banned <- getObjectValueWithDefault [] yamlObject "banned"
  premier <- getObjectValueWithDefault False yamlObject "premier"
  return $ Cup {
    name = name,
    little = little,
    evolve = evolve,
    allowed = allowed,
    excluded = excluded,
    pokemon = pokemon,
    banned = banned,
    premier = premier
    }
