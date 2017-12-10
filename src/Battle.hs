module Battle (
  Battle,
  Battle.init,
  runBattle,
  runBattleOnly,
  attacker,
  defender,
  dps,
  damageInflicted
) where

import qualified Action
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
import qualified Data.Tuple.Sequence as Tuple
import qualified System.Random as Random

import qualified Debug as D

data Battle = Battle {
  attacker :: Attacker,
  defender :: Defender,
  timer :: Int,
  initialDefenderHp :: Int
} deriving (Show)

battleDuration = 100 * 1000

init :: Pokemon -> Pokemon -> Battle
init attacker defender =
  let rnd = Random.mkStdGen 23
  in Battle {
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

runBattleOnly :: Battle -> Battle
runBattleOnly =
  fst . Writer.runWriter . runBattle

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

decorate :: (w -> u) -> Writer [w] a -> Writer [u] a
decorate f m =
  let (a, ws) = Writer.runWriter m
      us = map f ws
  in Writer.writer (a, us)

addOuch :: Writer [String] a -> Writer [Action] a
addOuch =
  decorate (Action . (++ ", ouch!"))

tick :: Battle -> Writer [Action] Battle
tick this = do
  let blah1 :: (Attacker, Defender) -> Writer [Action] (Attacker, Defender)
      blah1 (attacker, defender) =
        if Defender.damageWindow defender == 0
          then Tuple.sequenceT (
            addOuch $ Attacker.takeDamage
              (Defender.pokemon defender)
              (Defender.move defender)
              attacker,
            Defender.useEnergy defender)
          else return (attacker, defender)
      blah2 (attacker, defender) =
        if Attacker.damageWindow attacker == 0
          then Tuple.sequenceT (
            Attacker.useEnergy attacker,
            Defender.takeDamage
              (Attacker.pokemon attacker)
              (Attacker.move attacker)
              defender)
          else return (attacker, defender)
      doTick (attacker, defender) =
        return $ (Attacker.tick attacker, Defender.tick defender)
      makeMove (attacker, defender) =
        Tuple.sequenceT (Attacker.makeMove attacker, Defender.makeMove defender)
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
