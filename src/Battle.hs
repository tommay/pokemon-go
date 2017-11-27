module Battle (
  Battle,
  init,
  runBattle,
  dps,
  damageInflicted
) where

import qualified Attacker
import           Attacker (Attacker)
import qualified Defender
import           Defender (Defender)

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
  case find Battle.attackerFainted $ iiterate Battle.tick this of
    Just result -> result
    Nothing -> error "Shouldn't happen."

dps :: Battle -> Float
dps this =
  fronIntegral (Battle.damageInflicted this) / (Battle.secondsElapsed this)

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
tick battle =
  let attacker2 = Attacker.tick $ Battle.attacker this
      defender2 = Defender.tick $ Battle.defender this
      (attacker3, defender3) = if Defender.damageWindow defender2 == 0
        then (
          Attacker.takeDamage
            (Defender.pokemon defender2)
            (Defender.move defender2)
            attacker2,
          Defender.useEnergy defender2)
        else (attacker2, defender2)
      (attacker4, defender4) = if Attacker.damageWindow attacker3 == 0
        then (
          Attacker.useEnergy attacker3,
          Defender.takeDamage
            (Attacker.pokemon attacker3)
            (Attacker.move attacker3)
            defender3)
        else (attacker3, defender3)
  in battle {
    attacker = attacker4,
    defender = defender4,
    timer = Battle.timer battle - 10
    }
