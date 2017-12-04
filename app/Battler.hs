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
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}

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

import           Control.Applicative (optional, some)
import qualified Control.Monad.Writer as Writer
--import qualified Data.Attoparsec.Text as AP
import qualified Data.List as List
import qualified Data.Maybe as Maybe
--import qualified Data.Set as Set
--import qualified Data.Text as Text
import qualified System.IO as I
import qualified Text.Printf as Printf
--import qualified Text.Regex as Regex

defaultLevel = 20

data Options = Options {
  defender :: String
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

      defenders <- makeWithAllMovesetsFromSpecies gameMaster (defender options)

      attackers <- do
        mythicalMap <- do
          ioMythicalMap <- Mythical.load "mythical.yaml"
          ioMythicalMap
        let notMythical =
              not . Mythical.isMythical mythicalMap . PokemonBase.species
            allBases = GameMaster.allPokemonBases gameMaster
            attackerBases =
                filter notMythical
              $ filter (not . PokemonBase.hasEvolutions)
              allBases
            attackers = concat $ map
              (makeWithAllMovesetsFromBase gameMaster defaultLevel)
              attackerBases
        return attackers

      let attackerResults =
            [getAttackerResult defenders attacker | attacker <- attackers]

      let highDps = keepHighDpsSpecies attackerResults

      let byDamage = reverse $
            List.sortBy (compareWith minDamage) highDps

      mapM_ (putStrLn . showAttackerResult) byDamage
    )
    $ \ex -> I.hPutStrLn I.stderr ex

keepHighDpsSpecies :: [AttackerResult] -> [AttackerResult]
keepHighDpsSpecies attackerResults =
  let sortedByDps = reverse $ List.sortBy (compareWith dps) attackerResults
      highDps = take (length sortedByDps `div` 6) sortedByDps
      resultSpecies = Pokemon.species . pokemon
      keepSpecies = List.nub $ List.sort $ map resultSpecies highDps
  in filter (\ result -> resultSpecies result `elem` keepSpecies)
       attackerResults

{-
gyarados quick/charge defends against
  order by damage inflicted
  pidgey quick/charge dps damage inflicted
  pidgey quick/charge dps damage inflicted
  caterpie quick/charge dps damage inflicted
  caterpie quick/charge dps damage inflicted
-}

showAttackerResult :: AttackerResult -> String
showAttackerResult result =
  let attacker = showPokemon $ pokemon $ result
      minDamage' = minDamage result
      dps' = dps result
      maxDamage' = maxDamage result
  in Printf.printf "%-35s %.1f %d - %d" attacker dps' minDamage' maxDamage'

showPokemon :: Pokemon -> String
showPokemon pokemon =
  Printf.printf "%s %s / %s" (Pokemon.species pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)

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
       pokemon = attacker,
       battles = battles,
       minByDamage = minByDamage,
       minDamage = minDamage,
       dps = Battle.dps minByDamage,
       maxDamage = maxDamage
       }

{-
-- Results of a particular attacker species against all defender movesets.
-- Really we want to know the moveset with the highest minDamage because
-- it's the lower bound

data AttackerBaseResult = AttackerBaseResult {
  pokemonBase :: PokemonBase,
  attackerResults :: [AttackerResult],
  minByDamage :: AttackerResult,
  minDamage :: Int
}

getAttackerBaseResult :: GameMaster -> [Pokemon] -> PokemonBase -> AttackerBaseResult
getAttackerBaseResult gameMaster defenders attackerBase =
  let attackers =
        makeWithAllMovesetsFromBase gameMaster defaultLevel attackerBase
      attackerResults = [
        getAttackerResult defenders attacker | attacker <- attackers]
      minByDamage = List.minimumBy (compareWith minDamage) attackerResults
      minDamage = AttackerResult.minDamage minByDamage
  in AttackerBaseResult {
    pokemonBase = attackerBase,
    attackerResults = attackerResults,
    minByDamage = minByDamage,
    minDamage = minDamage
    }
-}
