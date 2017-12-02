module Battle (
  Battle,
  Battle.init,
  runBattle,
  dps,
  damageInflicted
) where

import           Action (Action (Action))
import qualified Attacker
import           Attacker (Attacker)
import qualified Defender
import           Defender (Defender)
import qualified Pokemon
import           Pokemon (Pokemon)

import qualified Control.Monad.Loops as Loops
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
--
runBattle :: Battle -> Writer [Action] Battle
runBattle =
  Loops.iterateUntilM Battle.attackerFainted Battle.tick

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
attackerFainted =
  Attacker.fainted . Battle.attacker

tick :: Battle -> Writer [Action] Battle
tick this = do
  Writer.tell [Action (timer this) "Clock tick"]
  let blah1 :: (Attacker, Defender) -> Writer [Action] (Attacker, Defender)
      blah1 (attacker, defender) =
        if Defender.damageWindow defender == 0
          then do
            Writer.tell [Action (timer this) "Attacker takes damage"]
            return (
              Attacker.takeDamage
                (Defender.pokemon defender)
                (Defender.move defender)
                attacker,
              Defender.useEnergy defender)
          else return (attacker, defender)
      blah2 (attacker, defender) =
        return $ if Attacker.damageWindow attacker == 0
          then (
            Attacker.useEnergy attacker,
            Defender.takeDamage
              (Attacker.pokemon attacker)
              (Attacker.move attacker)
              defender)
        else (attacker, defender)
      doTick (attacker, defender) =
        return $ (Attacker.tick attacker, Defender.tick defender)
      makeMove (attacker, defender) = 
        return $ (Attacker.makeMove attacker, Defender.makeMove defender)
      pair = (Battle.attacker this, Battle.defender this)
  pair <- doTick pair
  pair <- blah1 pair
  pair <- blah2 pair
  pair <- makeMove pair
  let (attacker, defender) = pair
  return $ this {
    attacker = attacker,
    defender = defender,
    timer = Battle.timer this - 10
    }
