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
  hiddenPowerType,
  chargeName,
  appraisal,
  stats,
  attack,
  defense,
  stamina,
  level,
  setStats
) where

import qualified Epic
import qualified Stats
import           Stats (Stats)

import qualified Data.Aeson.Types
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
  hiddenPowerType :: Maybe String,
  chargeName  :: String,
  appraisal   :: String,
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
          y .: "hp" <*>
          y .: "dust" <*>
          y .: "quick" <*>
          y .:? "hiddenPowerType" <*>
          y .: "charge" <*>
          y .: "appraisal" <*>
          y .:? "stats"

(.=?) :: (Builder.ToYaml a) => Text -> Maybe a -> [(Text, Builder.YamlBuilder)]
label .=? Nothing =
  []
label .=? Just a =
  [label .= a]

(.==) :: (Builder.ToYaml a) => Text -> a -> [(Text, Builder.YamlBuilder)]
label .== a =
  [label .= a]

instance Builder.ToYaml MyPokemon where
  toYaml this =
    Builder.mapping $ concat [
      "name" .== (Text.pack $ name this),
      "species" .== (Text.pack $ species this),
      "cp" .== cp this,
      "hp" .== hp this,
      "dust" .== stardust this,
      "quick" .== (Text.pack $ quickName this),
      "hiddenPowerType" .=? (Text.pack <$> hiddenPowerType this),
      "charge" .== (Text.pack $ chargeName this),
      "appraisal" .== (Text.pack $ appraisal this),
      "stats" .=? stats this
    ]

instance (Builder.ToYaml a) => Builder.ToYaml (Maybe a) where
  toYaml Nothing = undefined
  toYaml (Just a) = Builder.toYaml a

load :: Epic.MonadCatch m => FilePath -> IO (m [MyPokemon])
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Right myPokemon -> return $ pure myPokemon
    Left yamlParseException ->
      Epic.fail $ Yaml.prettyPrintParseException yamlParseException

level :: Epic.MonadCatch m => MyPokemon -> m Float
level = getStat Stats.level

attack :: Epic.MonadCatch m => MyPokemon -> m Int
attack = getStat Stats.attack

defense :: Epic.MonadCatch m => MyPokemon -> m Int
defense = getStat Stats.defense

stamina :: Epic.MonadCatch m => MyPokemon -> m Int
stamina = getStat Stats.stamina

getStat :: (Num a, Epic.MonadCatch m) => (Stats -> a) -> MyPokemon -> m a
getStat getter this = do
  case MyPokemon.stats this of
    Just (stats:_) -> return $ getter stats
    Nothing -> Epic.fail $ "No stats for " ++ (MyPokemon.name this)

setStats :: MyPokemon -> [Stats] -> MyPokemon
setStats this stats =
  this { stats = Just stats }
