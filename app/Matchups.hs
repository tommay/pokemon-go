{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import           Battle (Battle)
import qualified Epic
import qualified IVs
import           IVs (IVs)
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Logger
import qualified Move
import           Move (Move)
import qualified MakePokemon
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Mythical
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Util

import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

import           Control.Monad (join)
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified System.IO as I

defaultIvs = IVs.new 20 11 11 11

data Options = Options {
  level :: Float,
  attackersFile :: Maybe FilePath
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLevel <*> optAttackers
      optLevel = O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.metavar "LEVEL"
        <> O.value (IVs.level defaultIvs)
        <> O.showDefault
        <> O.help ("Set the level of the battling pokemon"))
      optAttackers = O.optional $ O.strOption
        (  O.long "attackers"
        <> O.short 'a'
        <> O.metavar "ATTACKERS-FILE"
        <> O.help "File with attacking pokemon, default all")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Simulate matchups between all pokemon")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      mythicalMap <- join $ Mythical.load "mythical.yaml"

      let notMythical =
            not . Mythical.isMythical mythicalMap . PokemonBase.species
          notLegendary =
            not . Mythical.isLegendary mythicalMap . PokemonBase.species

          ivs = IVs.setLevel defaultIvs $ level options

          allBases = GameMaster.allPokemonBases gameMaster

      attackers <- case attackersFile options of
        Just filename -> do
          myPokemon <- join $ MyPokemon.load $ Just filename
          mapM (fmap head . MakePokemon.makePokemon gameMaster) myPokemon
        Nothing -> do
          let attackerBases =
                  filter notMythical
                $ filter notLegendary
                $ filter (not . PokemonBase.hasEvolutions)
                allBases
          return $ concat $ map
            (MakePokemon.makeWithAllMovesetsFromBase gameMaster ivs)
            attackerBases

      let defenderBases =
              filter notMythical
            $ filter (not . PokemonBase.hasEvolutions)
            allBases

          defenderSets =
            map (MakePokemon.makeWithAllMovesetsFromBase gameMaster ivs)
            defenderBases

          attackerResults = [getAttackerResult defenders attacker |
            defenders <- defenderSets, attacker <- attackers]

      I.hSetBuffering I.stdout I.NoBuffering

      -- Using mapM_ to output the individual array elements (as
      -- arrays of one element) instead of writing the entire array
      -- allows the results to stream.

      mapM_ (B.putStr . Builder.toByteString . (:[])) attackerResults
    )
    $ I.hPutStrLn I.stderr

-- Results of a particular attacker/moveset against all defender
-- movesets.  The pokemon is expected to score at least minDamage no
-- matter what the defender's moveset is.

data AttackerResult = AttackerResult {
  defender :: String,
  pokemon :: Pokemon,
  battles :: [Battle],
  minByDamage :: Battle,
  minDamage :: Int,
  dps       :: Float,
  maxDamage :: Int
}

getAttackerResult :: [Pokemon] -> Pokemon -> AttackerResult
getAttackerResult defenders attacker =
  let battleLoggers = [
        Battle.runBattle $ Battle.init (const 1) attacker defender |
        defender <- defenders]
      battles = map (fst . Logger.runLogger) battleLoggers
      minByDamage =
        List.minimumBy (Util.compareWith Battle.damageInflicted) battles
      minDamage = Battle.damageInflicted minByDamage
      maxByDamage =
        List.maximumBy (Util.compareWith Battle.damageInflicted) battles
      maxDamage = Battle.damageInflicted maxByDamage
  in AttackerResult {
       defender = Pokemon.species $ head defenders,
       pokemon = attacker,
       battles = battles,
       minByDamage = minByDamage,
       minDamage = minDamage,
       dps = Battle.dps minByDamage,
       maxDamage = maxDamage
       }

(.==) :: (Builder.ToYaml a) => Text -> a -> [(Text, Builder.YamlBuilder)]
label .== a =
  [label .= a]

instance Builder.ToYaml AttackerResult where
  toYaml this =
    Builder.mapping $ concat [
      "defender" .== (Text.pack $ defender this),
      "attacker" .== (Text.pack $ Pokemon.pname $ pokemon this),
      "quick" .== (Text.pack $ Move.name $ Pokemon.quick $ pokemon this),
      "charge" .== (Text.pack $ Move.name $ Pokemon.charge $ pokemon this),
      "dps" .== (dps this),
      "minDamage" .== (minDamage this),
      "maxDamage" .== (maxDamage this)
    ]
