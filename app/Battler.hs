{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Attacker
import           Attacker (Attacker)
import qualified Battle
import           Battle (Battle)
import qualified BattlerUtil
import           BattlerUtil (Battler)
import qualified Defender
import           Defender (Defender)
import qualified Epic
import qualified Friend
import           Friend (Friend)
import qualified IVs
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Log
import           Log (Log)
import qualified Logger
import           Logger (Logger)
import qualified Move
import           Move (Move)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil
import qualified Weather
import           Weather (Weather (..))

import           Control.Applicative (optional, some)
import           Control.Monad (join)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

import qualified Debug

data Options = Options {
  maybeWeather :: Maybe Weather,
  maybeFriend :: Maybe Friend,
  attacker :: Battler,
  defender :: Battler
} deriving (Show)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optWeather <*> optFriend <*>
        optAttacker <*> optDefender
      optWeather = O.optional Weather.optWeather
      optFriend = O.optional Friend.optFriend
      optAttacker = O.argument
        BattlerUtil.optParseBattler
        (O.metavar "ATTACKER[:LEVEL]")
      optDefender = O.argument
        BattlerUtil.optParseBattler
        (O.metavar "DEFENDER[:LEVEL]")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Battle some pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load

      attackerVariants <-
        BattlerUtil.makeBattlerVariants gameMaster (attacker options)
      defenderVariants <-
        BattlerUtil.makeBattlerVariants gameMaster (defender options)

      let weatherBonus =
            GameMaster.getWeatherBonus gameMaster $ maybeWeather options
          friendBonus = Friend.damageBonus $ maybeFriend options
          makeBattle attacker defender = Battle.init attacker defender
            `Battle.setWeatherBonus` weatherBonus
            `Battle.setFriendBonus` friendBonus
          battleLoggers =
            [Battle.doBattle $ makeBattle attacker defender |
              attacker <- attackerVariants, defender <- defenderVariants]
          battleResults = map Logger.runLogger battleLoggers

      putStrLn $ List.intercalate "\n" $ map showBattle battleResults
    )
    $ Exit.die

tell :: a -> Logger a ()
tell = Logger.log

-- Using Logger here is just a "convenient" wat to collect a bunch of
-- disparate strings into a List.  It might be more clear just to : or
-- ++ the List together.
showBattle :: (Battle, [Log Battle]) -> String
showBattle (battle, logs) = 
  List.intercalate "\n" $
  Logger.execLogger (
    do
      tell $ showPokemon $ Attacker.pokemon $ Battle.attacker battle
      tell $ showPokemon $ Defender.pokemon $ Battle.defender battle
      mapM_ (tell . showLog) logs)

showLog :: Log Battle -> String
showLog log =
  let battle = Log.state log
      when = Battle.secondsElapsed battle
      attacker = Battle.attacker battle
      defender = Battle.defender battle
  in Printf.printf "%.3f: %3d (%d/%d) %3d (%d/%d/%d) - %3d (%d/%d) %3d (%d/%d/%d): %s"
       when
       (Attacker.hp attacker)
       (Attacker.quickDamage attacker)
       (Attacker.chargeDamage attacker)
       (Attacker.energy attacker)
       (Attacker.quickEnergy attacker)
       (Attacker.damageEnergy attacker)
       (Attacker.wastedEnergy attacker)
       (Defender.hp defender)
       (Defender.quickDamage defender)
       (Defender.chargeDamage defender)
       (Defender.energy defender)
       (Defender.quickEnergy defender)
       (Defender.damageEnergy defender)
       (Defender.wastedEnergy defender)
       (Log.what log)

showPokemon :: Pokemon -> String
showPokemon pokemon =
  Printf.printf "%s:%s/%d/%d/%d:%s/%s"
    (Pokemon.species pokemon)
    (PokeUtil.levelToString $ Pokemon.level pokemon)
    (IVs.attack $ Pokemon.ivs pokemon)
    (IVs.defense $ Pokemon.ivs pokemon)
    (IVs.stamina $ Pokemon.ivs pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)
