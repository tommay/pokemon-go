{-# LANGUAGE OverloadedStrings #-}

-- Simulates matchups between all attackers and defenders where attackers
-- are at the top of their evolution chain and defenders are all tier 3
-- raid bosses.
-- Alternatively a file of attackers can be given and they will be
-- simulated against all defenders.
--
-- For each defender, consider all movesets and output the worst case
-- dps and the min and max damage for each of the attacker's movesets.
--
-- The output can be used by "elites" to figure which pokemon are the
-- best attackers.

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import           Battle (Battle)
import qualified BattlerUtil
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
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)

import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

import           Control.Monad (join)
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified System.IO as IO
import qualified System.Exit as Exit

import qualified Debug

defaultIvs = IVs.defaultIVs

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
        <> O.help "Set the level of the attackers")
      optAttackers = O.optional $ O.strOption
        (  O.long "attackers"
        <> O.short 'a'
        <> O.metavar "ATTACKERS-FILE"
        <> O.help "File with attacking pokemon, default all")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc
          "Simulate matchups between all pokemon against tier 3 raid bosses")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      let ivs = IVs.setLevel defaultIvs $ level options

          allBases = GameMaster.allPokemonBases gameMaster

      attackers <- case attackersFile options of
        Just filename -> do
          myPokemon <- join $ MyPokemon.load $ Just filename
          mapM (fmap head . MakePokemon.makePokemon gameMaster) myPokemon
        Nothing -> do
          let attackerBases =
                filter (not . PokemonBase.hasEvolutions)
                  allBases
          return $ concat $ map
            (MakePokemon.makeWithAllMovesetsFromBase gameMaster ivs)
            attackerBases

      -- Smeargle's moves are handled strangely in GAME_MASTER.yaml and
      -- it currently ends up with an empty move list which causes head to
      -- fail.  For now, filter it out here.

      let defenderBases =
            filter ((/= "smeargle") . PokemonBase.species) allBases

          defenderSets =
            map (BattlerUtil.makeRaidBossForTier gameMaster 3) defenderBases

          attackerResults = [getAttackerResult defenders attacker |
            defenders <- defenderSets, attacker <- attackers]

      IO.hSetBuffering IO.stdout IO.NoBuffering

      -- Using mapM_ to output the individual array elements (as
      -- arrays of one element) instead of writing the entire array
      -- allows the results to stream.

      mapM_ (B.putStr . Builder.toByteString . (:[])) attackerResults
    )
    $ Exit.die

-- Results of a particular attacker/moveset against all defender
-- movesets.  The pokemon is expected to score at least minDamage no
-- matter what the defender's moveset is.

data AttackerResult = AttackerResult {
  defender :: String,
  pokemon :: Pokemon,
  battles :: [Battle],
  minDamage :: Int,
  dps       :: Float,
  maxDamage :: Int
}

getAttackerResult :: [Pokemon] -> Pokemon -> AttackerResult
getAttackerResult defenders attacker =
  let battleLoggers = [
        Battle.doBattle (const 1) False attacker defender |
        defender <- defenders]
      battles = map (fst . Logger.runLogger) battleLoggers
      minDamage = Battle.damageInflicted $
        List.minimumBy (Ord.comparing Battle.damageInflicted) battles
      maxDamage = Battle.damageInflicted $
        List.maximumBy (Ord.comparing Battle.damageInflicted) battles
      dps = Battle.dps $
        List.minimumBy (Ord.comparing Battle.dps) battles

  in AttackerResult {
       defender = Pokemon.species $ head defenders,
       pokemon = attacker,
       battles = battles,
       minDamage = minDamage,
       dps = dps,
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
