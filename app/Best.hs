-- What do I want to know?  The defender always has an unknown moveset
-- so the answers will always come from AttackerResult which takes
-- the worst case over all defender movesets.
--
-- What are the most effective species/moveset attackers?
--   Sort [AttackerResult] by minDamage and take the top N.
-- What are the top attackers regardless of moveset?

-- For a given attacker, what is it most effective against?
-- For a given attacker/movsset, what is it most effective against?
-- How "good" is a given attacker apecies?
-- How "good" is a given attacker apecies with a given movset?

{-# LANGUAGE OverloadedStrings #-}
-- I think OverloadedRecordFieldsis what I really want but it's not
-- fully implemented and therefore doesn't exist.  In the meantime,
-- DuplicateRecordFields allows record fields to have the same name
-- and automatically enabled DisambiguateRecordFields, but the
-- disambiguation is not automatic and requires manually annotating
-- uses which is not much better than just giving them distinct names.
--{-# LANGUAGE OverloadedRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Action
import qualified Attacker
import           Attacker (Attacker)
import qualified Battle
import           Battle (Battle)
import qualified Defender
import           Defender (Defender)
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import           Move (Move)
import qualified Mythical
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Mythical
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Type
import           Type (Type)

import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

import           Control.Applicative (optional, some)
import qualified Control.Monad.Writer as Writer
--import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Maybe as Maybe
--import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified System.IO as I
import qualified Text.Printf as Printf
--import qualified Text.Regex as Regex

defaultLevel = 20

data Options = Options {
  xdefender :: String
}

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optDefender
      optDefender = O.argument O.str (O.metavar "DEFENDER")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Battle some pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- do
        ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
        ioGameMaster

      defenders <- makeWithAllMovesetsFromSpecies gameMaster (xdefender options)

      attackers <- do
        mythicalMap <- do
          ioMythicalMap <- Mythical.load "mythical.yaml"
          ioMythicalMap
        let notMythical =
              not . Mythical.isMythical mythicalMap . PokemonBase.species
            notLegendary =
              not . Mythical.isLegendary mythicalMap . PokemonBase.species
            allBases = GameMaster.allPokemonBases gameMaster
            attackerBases =
                filter notMythical
              $ filter notLegendary
              $ filter (not . PokemonBase.hasEvolutions)
              allBases
            attackers = concat $ map
              (makeWithAllMovesetsFromBase gameMaster defaultLevel)
              attackerBases
        return attackers

      let attackerResults =
            [getAttackerResult defenders attacker | attacker <- attackers]

      B.putStr $ Builder.toByteString attackerResults
    )
    $ \ex -> I.hPutStrLn I.stderr ex

compareWith :: Ord b => (a -> b) -> a -> a -> Ordering
compareWith f first second =
  f first `compare` f second

makeWithAllMovesetsFromSpecies :: Epic.MonadCatch m =>
    GameMaster -> String -> m [Pokemon]
makeWithAllMovesetsFromSpecies gameMaster species = do
  base <- GameMaster.getPokemonBase gameMaster species
  return $ makeWithAllMovesetsFromBase gameMaster defaultLevel base

makeWithAllMovesetsFromBase :: GameMaster -> Float -> PokemonBase -> [Pokemon]
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
        List.minimumBy (compareWith Battle.damageInflicted) battles
      minDamage = Battle.damageInflicted minByDamage
      maxByDamage =
        List.maximumBy (compareWith Battle.damageInflicted) battles
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
      "dps" .== (dps this),
      "minDamage" .== (minDamage this),
      "maxDamage" .== (maxDamage this)
    ]
