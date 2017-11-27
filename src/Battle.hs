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

import qualified Data.List as List

data Battle = Battle {
  attacker :: Attacker,
  defender :: Defender,
  timer :: Int,
  initialDefenderHp :: Int
  }

battleDuration = 100 * 1000

init :: Attacker -> Defender -> Battle
init attacker defender =
  Battle {
    attacker = attacker,
    defender = defender,
    timer = battleDuration,
    initialDefenderHp = Defender.hp defender
    }

-- Given an initial Battle state, run a battle and return the final state
-- when Battle.finished is true.
----
runBattle :: Battle -> Battle
runBattle this =
  case List.find Battle.attackerFainted $ iterate Battle.tick this of
    Just result -> result
    Nothing -> error "Shouldn't happen."

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

tick :: Battle -> Battle
tick this =
  let doTick attacker defender =
        (Attacker.tick attacker, Defender.tick defender)
      blah1 (attacker, defender) =
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
      (attacker, defender) =
        blah2 $ blah1 $ doTick (Battle.attacker this) (Battle.defender this)
  in this {
    attacker = attacker,
    defender = defender,
    timer = Battle.timer this - 10
    }
