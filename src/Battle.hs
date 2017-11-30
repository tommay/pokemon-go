module Battle (
  Battle,
  Battle.init,
  runBattle,
  dps,
  damageInflicted
) where

import qualified Attacker
import           Attacker (Attacker)
import qualified Defender
import           Defender (Defender)
import qualified Pokemon
import           Pokemon (Pokemon)

import qualified Control.Monad.Writer as Writer
import           Control.Monad.Writer (Writer)
import qualified Data.List as List
import qualified System.Random as Random

import qualified Debug as D

data Battle = Battle {
  attacker :: Attacker,
  defender :: Defender,
  timer :: Int,
  initialDefenderHp :: Int
} deriving (Show)

data Action = Action {
  clock :: Int,
  what :: String,
  after :: Battle
} deriving (Show)

battleDuration = 100 * 1000

init :: Random.StdGen -> Pokemon -> Pokemon -> Battle
init rnd attacker defender =
  Battle {
    attacker = Attacker.init attacker,
    defender = Defender.init rnd defender,
    timer = battleDuration,
    initialDefenderHp = Pokemon.hp defender * 2
    }

-- Given an initial Battle state, run a battle and return the final state
-- when Battle.finished is true.
----
runBattle :: Battle -> Writer [Action] Battle
runBattle this = do
  battle <- tick this
  if Battle.attackerFainted battle
    then return battle
    else runBattle battle

-- XXX This is not quite right because there is some delay before the
-- attacker's first move, so the dps only starts after the delay.
--
dps :: Battle -> Float
dps this =
  fromIntegral (Battle.damageInflicted this) / Battle.secondsElapsed this

damageInflicted :: Battle -> Int
damageInflicted this =
  Battle.initialDefenderHp this - Defender.hp (Battle.defender this)

secondsElapsed :: Battle -> Float
secondsElapsed this =
  fromIntegral (battleDuration - Battle.timer this) / 1000

-- The Battle is run until the attacker faints.  This lets us get a dps
-- and a total inflicted damage which allows for tankiness.
--
attackerFainted :: Battle -> Bool
attackerFainted this =
  Attacker.fainted $ Battle.attacker this

tick :: Battle -> Writer [Action] Battle
tick this = do
  let blah1 (attacker, defender) =
        if Defender.damageWindow defender == 0
          then (
            Attacker.takeDamage
              (Defender.pokemon defender)
              (Defender.move defender)
              attacker,
            Defender.useEnergy defender)
          else (attacker, defender)
      blah2 (attacker, defender) =
        if Attacker.damageWindow attacker == 0
          then (
            Attacker.useEnergy attacker,
            Defender.takeDamage
              (Attacker.pokemon attacker)
              (Attacker.move attacker)
              defender)
        else (attacker, defender)
      doTick (attacker, defender) =
        (Attacker.tick attacker, Defender.tick defender)
      makeMove (attacker, defender) =
        (Attacker.makeMove attacker, Defender.makeMove defender)
      (attacker, defender) =
        ((Battle.attacker this), (Battle.defender this))
          |> doTick |> blah1 |> blah2 |> makeMove
      result = this {
        attacker = attacker,
        defender = defender,
        timer = Battle.timer this - 10
        }
  Writer.tell [Action (timer this) "Clock tick" result]
  return result

-- The |> operator lets us send a piecce of data through a function
-- pipeline.
--
infixl 0 |>
(|>) :: a -> (a -> b) -> b
val |> func = func val
