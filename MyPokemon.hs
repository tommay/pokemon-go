-- So .: works with Strings?
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Yaml as Yaml
import Data.Yaml (FromJSON(..), (.:))  -- ???

data Pokemon = Pokemon {
  name        :: String,
  species     :: String,
  cp          :: Integer
} deriving (Show)

data Stat = Stat {
  level       :: Integer,
  attack      :: Integer,
  defense     :: Integer,
  stamins     :: Integer
} deriving (Show)

instance Yaml.FromJSON Pokemon where
  parseJSON (Yaml.Object y) =
    Pokemon <$>
    y .: "name" <*>
    y .: "species" <*>
    y .: "cp"
  parseJSON _ = fail "Expected Yaml.Object for Pokemon.parseJSON"

main :: IO ()
main = poke "my_pokemon.yaml"

poke :: FilePath -> IO ()
poke filename = do
  yamlResult <- Yaml.decodeFileEither filename ::
    IO (Either Yaml.ParseException [Pokemon])
  case yamlResult of
    Right myPokemon ->
      mapM_ print myPokemon
    Left exception ->
      print exception
