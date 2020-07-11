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
  quickName,
  chargeName,
  maybeChargeName2,
  ivs,
  stats,
  level,
  setIVs,
  setStats,
  setLevel,
  setSpecies,
  setQuickName,
  setChargeName,
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
  quickName   :: String,
  chargeName  :: String,
  maybeChargeName2 :: Maybe String,
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
          y .: "quick" <*>
          y .: "charge" <*>
          y .:? "charge2" <*>
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
      "quick" .== (Text.pack $ quickName this),
      "charge" .== (Text.pack $ chargeName this),
      "charge2" .=? (Text.pack <$> maybeChargeName2 this),
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

setQuickName :: String -> MyPokemon -> MyPokemon
setQuickName quickName this =
  this { quickName = quickName }

setChargeName :: String -> MyPokemon -> MyPokemon
setChargeName chargeName this =
  this { chargeName = chargeName }
