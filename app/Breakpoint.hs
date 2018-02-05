{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import qualified BattlerUtil
import           BattlerUtil (Battler)
import qualified Epic
import qualified IVs
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Pokemon
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
  attacker :: Battler,
  defender :: Battler
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optWeather <*> optAttacker <*> optDefender
      optWeather = O.optional Weather.optWeather
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

      let attacker = Main.attacker options

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

      forM_ (take 20 breakpoints) $ \ (level, damage) ->
        putStrLn $ Printf.printf "%4.1f %d" level damage
    )
    $ I.hPutStrLn I.stderr
