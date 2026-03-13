-- So .: works with Strings.
{-# LANGUAGE OverloadedStrings #-}

module MyPokemon (
  MyPokemon (MyPokemon),
  load,
  name,
  species,
  cp,
  hp,
  stardust,
  fastName,
  chargedName,
  maybeChargedName2,
  chargedNames,
  ivs,
  stats,
  level,
  setIVs,
  setStats,
  setLevel,
  setSpecies,
  setFastName,
  setChargedName,
) where

import qualified Epic
import qualified IVs
import           IVs (IVs)
import qualified Stats
import           Stats (Stats)
import qualified Util
import           YamlUtil ((.=?), (.==))

import qualified Data.Aeson.Types
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Data.Maybe as Maybe
import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:), (.:?))
import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

import qualified Data.Text as Text
import           Data.Text (Text)

data MyPokemon = MyPokemon {
  name        :: String,
  species     :: String,
  cp          :: Int,
  hp          :: Int,
  stardust    :: Int,
  fastName    :: String,
  chargedName :: String,
  maybeChargedName2 :: Maybe String,
  ivs         :: IVs,
  stats       :: Maybe Stats
} deriving (Show)

instance Yaml.FromJSON MyPokemon where
  parseJSON = Yaml.withObject "Pokemon" $ \y -> do
    name <- y .: "name"
    Data.Aeson.Types.modifyFailure
      (("Error parsing " ++ name ++ ": ") ++)
      $ MyPokemon <$>
          pure name <*>
          y .: "species" <*>
          y .: "cp" <*>
          y .: "hp" <*>
          y .: "dust" <*>
          y .: "fast" <*>
          y .: "charged" <*>
          y .:? "charged2" <*>
          y .: "ivs" <*>
          y .:? "stats"

instance Builder.ToYaml MyPokemon where
  toYaml this =
    Builder.mapping $ Maybe.catMaybes [
      "name" .== (Text.pack $ name this),
      "species" .== (Text.pack $ species this),
      "cp" .== cp this,
      "hp" .== hp this,
      "dust" .== stardust this,
      "fast" .== (Text.pack $ fastName this),
      "charged" .== (Text.pack $ chargedName this),
      "charged2" .=? (Text.pack <$> maybeChargedName2 this),
      "ivs" .== ivs this,
      "stats" .=? stats this
    ]

instance (Builder.ToYaml a) => Builder.ToYaml (Maybe a) where
  toYaml Nothing = undefined
  toYaml (Just a) = Builder.toYaml a

load :: Epic.MonadCatch m => Maybe FilePath -> IO (m [MyPokemon])
load maybeFilepath = do
  byteString <- Util.toByteString maybeFilepath
  case Yaml.decodeEither' byteString of
    Right myPokemon -> return $ pure myPokemon
    Left yamlParseException ->
      Epic.fail $ Yaml.prettyPrintParseException yamlParseException

chargedNames :: MyPokemon -> [String]
chargedNames this =
  Maybe.catMaybes [Just $ chargedName this, maybeChargedName2 this]

level :: MyPokemon -> Float
level = getIv IVs.level

getIv :: Num a => (IVs -> a) -> MyPokemon -> a
getIv getter =
  getter . ivs

setIVs :: MyPokemon -> IVs -> MyPokemon
setIVs this ivs =
  this { ivs = ivs }

setStats :: MyPokemon -> Stats -> MyPokemon
setStats this stats =
  this { stats = Just stats }

setLevel :: MyPokemon -> Float -> MyPokemon
setLevel this level =
  setIVs this $ IVs.setLevel (ivs this) level

setSpecies :: MyPokemon -> String -> MyPokemon
setSpecies this species =
  this { species = species }

setFastName :: String -> MyPokemon -> MyPokemon
setFastName fastName this =
  this { fastName = fastName }

setChargedName :: String -> MyPokemon -> MyPokemon
setChargedName chargedName this =
  this { chargedName = chargedName }
