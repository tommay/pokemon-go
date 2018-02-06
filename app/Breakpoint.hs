{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import qualified BattlerUtil
import           BattlerUtil (Battler, Level (Level))
import qualified Epic
import qualified IVs
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import qualified Util
import qualified Weather
import           Weather (Weather (..))

import           Control.Applicative (optional, some)
import           Control.Monad (join, forM, forM_)
import qualified Data.List as List
import qualified System.IO as I
import qualified Text.Printf as Printf

defaultIVs = IVs.new 20 11 11 11

data Options = Options {
  maybeWeather :: Maybe Weather,
  maybeFilename :: Maybe String,
  attacker :: Battler,
  defender :: Battler
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optWeather
        <*> optFilename <*> optAttacker <*> optDefender
      optWeather = O.optional Weather.optWeather
      optFilename = O.optional $ O.strOption
        (  O.long "file"
        <> O.short 'f'
        <> O.metavar "FILE"
        <> O.help "File to read my_pokemon from to get the attacker")
      optAttacker = O.argument
        (BattlerUtil.parseBattler defaultIVs) (O.metavar "ATTACKER[:LEVEL]")
      optDefender = O.argument
        (BattlerUtil.parseBattler defaultIVs) (O.metavar "DEFENDER[:LEVEL]")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Battle some pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      let weatherBonus = case maybeWeather options of
            Just weather -> GameMaster.getWeatherBonus gameMaster weather
            Nothing -> const 1

      attacker <- case maybeFilename options of
        Just filename -> do
          myPokemon <- join $ MyPokemon.load filename
          let name = BattlerUtil.species $ Main.attacker options
          case filter ((Util.matchesAbbrevInsensitive name) . MyPokemon.name)
              myPokemon of
            [myPokemon] -> BattlerUtil.fromMyPokemon myPokemon
            [] -> Epic.fail $ "Can't find pokemon named " ++ name
            _ -> Epic.fail $ "Multiple pokemon named " ++ name
        Nothing -> return $ Main.attacker options

      defenderVariants <-
        BattlerUtil.makeBattlerVariants gameMaster (defender options)

      let defender = head defenderVariants
          levels = GameMaster.allLevels gameMaster

      levelAndDamage <- forM levels $ \level -> do
        let attacker' = BattlerUtil.setLevel level attacker
        attackerVariants  <-
          BattlerUtil.makeBattlerVariants gameMaster attacker'
        let attacker' = head attackerVariants
            damage = Battle.getDamage weatherBonus
              attacker' (Pokemon.quick attacker') defender
        return (level, damage)

      let breakpoints =
            List.nubBy (\ (_, d1) (_, d2) -> d1 == d2) levelAndDamage

      forM_ breakpoints $ \ (level, damage) ->
        putStrLn $ Printf.printf "%4.1f %d" level damage
    )
    $ I.hPutStrLn I.stderr
