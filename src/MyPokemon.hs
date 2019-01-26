-- So .: works with Strings.
{-# LANGUAGE OverloadedStrings #-}

module MyPokemon (
  MyPokemon (MyPokemon),
  load,
  name,
  species,
  cp,
  powerup,
  hp,
  stardust,
  quickName,
  chargeName,
  appraisal,
  ivs,
  stats,
  level,
  setName,
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
  powerup     :: Maybe Int,
  hp          :: Int,
  stardust    :: Int,
  quickName   :: String,
  chargeName  :: String,
  appraisal   :: String,
  ivs         :: Maybe [IVs],
  stats       :: Maybe [Stats]
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
          y .:? "powerup" <*>
          y .: "hp" <*>
          y .: "dust" <*>
          y .: "quick" <*>
          y .: "charge" <*>
          y .: "appraisal" <*>
          y .:? "ivs" <*>
          y .:? "stats"

(.=?) :: (Builder.ToYaml a) => Text -> Maybe a -> Maybe (Text, Builder.YamlBuilder)
label .=? Nothing =
  Nothing
label .=? Just a =
  Just $ label .= a

(.==) :: (Builder.ToYaml a) => Text -> a -> Maybe (Text, Builder.YamlBuilder)
label .== a =
  Just $ label .= a

instance Builder.ToYaml MyPokemon where
  toYaml this =
    Builder.mapping $ Maybe.catMaybes [
      "name" .== (Text.pack $ name this),
      "species" .== (Text.pack $ species this),
      "cp" .== cp this,
      case ivs this of
        Just [_] -> Nothing
        _ -> "powerup" .=? powerup this,
      "hp" .== hp this,
      "dust" .== stardust this,
      "quick" .== (Text.pack $ quickName this),
      "charge" .== (Text.pack $ chargeName this),
      "appraisal" .== (Text.pack $ appraisal this),
      "ivs" .=? ivs this,
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

level :: Epic.MonadCatch m => MyPokemon -> m Float
level = getIv IVs.level

getIv :: (Num a, Epic.MonadCatch m) => (IVs -> a) -> MyPokemon -> m a
getIv getter this = do
  case MyPokemon.ivs this of
    Just (ivs:_) -> return $ getter ivs
    Nothing -> Epic.fail $ "No ivs for " ++ (MyPokemon.name this)

setName :: MyPokemon -> String -> MyPokemon
setName this name =
  this { name = name }

setIVs :: MyPokemon -> Maybe [IVs] -> MyPokemon
setIVs this ivs =
  this { ivs = ivs }

setStats :: MyPokemon -> [Stats] -> MyPokemon
setStats this stats =
  this { stats = Just stats }

setLevel :: MyPokemon -> Float -> MyPokemon
setLevel this level =
  case MyPokemon.ivs this of
    Nothing -> this
    Just ivs ->
      setIVs this $ Just $ map (flip IVs.setLevel $ level) ivs

setSpecies :: MyPokemon -> String -> MyPokemon
setSpecies this species =
  this { species = species }

setQuickName :: String -> MyPokemon -> MyPokemon
setQuickName quickName this =
  this { quickName = quickName }

setChargeName :: String -> MyPokemon -> MyPokemon
setChargeName chargeName this =
  this { chargeName = chargeName }
