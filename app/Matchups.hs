{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Battle
import           Battle (Battle)
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import           Move (Move)
import qualified Mythical
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Util

import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

import qualified Control.Monad.Writer as Writer
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified System.IO as I

defaultLevel = 20

main =
  Epic.catch (
    do
      gameMaster <- do
        ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
        ioGameMaster

      mythicalMap <- do
        ioMythicalMap <- Mythical.load "mythical.yaml"
        ioMythicalMap

      let notMythical =
            not . Mythical.isMythical mythicalMap . PokemonBase.species
          notLegendary =
            not . Mythical.isLegendary mythicalMap . PokemonBase.species

      let allBases = GameMaster.allPokemonBases gameMaster

      let attackers =
            let attackerBases =
                    filter notMythical
                  $ filter notLegendary
                  $ filter (not . PokemonBase.hasEvolutions)
                  allBases
            in concat $ map
                 (makeWithAllMovesetsFromBase gameMaster defaultLevel)
                 attackerBases

      let defenderBases =
              filter notMythical
            $ filter (not . PokemonBase.hasEvolutions)
            allBases

      let defenderSets =
            map (makeWithAllMovesetsFromBase gameMaster defaultLevel) defenderBases

      let attackerResults = [getAttackerResult defenders attacker |
            defenders <- defenderSets, attacker <- attackers]

      I.hSetBuffering I.stdout I.NoBuffering

      -- Using mapM_ to output the individual array elements instead
      -- of writing the entire array allows the results to stream,
      -- but requires some post processing to add the yaml array syntax.
      mapM_ (B.putStr . Builder.toByteString) attackerResults
      -- B.putStr $ Builder.toByteString attackerResults
    )
    $ \ex -> I.hPutStrLn I.stderr ex

makeWithAllMovesetsFromBase gameMaster level base =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      makeStat baseStat = (fromIntegral baseStat + 11) * cpMultiplier
      makeAttacker quickMove chargeMove =
        Pokemon.new
          (PokemonBase.species base)
          (PokemonBase.species base)
          level
          (PokemonBase.types base)
          (makeStat $ PokemonBase.attack base)
          (makeStat $ PokemonBase.defense base)
          (makeStat $ PokemonBase.stamina base)
          quickMove
          chargeMove
          base
  in [makeAttacker quickMove chargeMove |
      (quickMove, chargeMove) <-
        PokemonBase.moveSets base]

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
  let battleWriters = [Battle.runBattle $ Battle.init attacker defender |
        defender <- defenders]
      battles = map (fst . Writer.runWriter) battleWriters
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
      "attacker" .== (Text.pack $ Pokemon.species $ pokemon this),
      "quick" .== (Text.pack $ Move.name $ Pokemon.quick $ pokemon this),
      "charge" .== (Text.pack $ Move.name $ Pokemon.charge $ pokemon this),
      "dps" .== (Text.pack $ show $ dps this),
      "minDamage" .== (minDamage this),
      "maxDamage" .== (maxDamage this)
    ]
