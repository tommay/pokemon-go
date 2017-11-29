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

import qualified Data.List as List

data Battle = Battle {
  attacker :: Attacker,
  defender :: Defender,
  timer :: Int,
  initialDefenderHp :: Int
  }

battleDuration = 100 * 1000

init :: Pokemon -> Pokemon -> Battle
init attacker defender =
  Battle {
    attacker = Attacker.init attacker,
    defender = Defender.init defender,
    timer = battleDuration,
    initialDefenderHp = Pokemon.hp defender
    }

-- Given an initial Battle state, run a battle and return the final state
-- when Battle.finished is true.
----
runBattle :: Battle -> Battle
runBattle this =
  until Battle.attackerFainted Battle.tick this

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
  let doTick (attacker, defender) =
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
        ((Battle.attacker this), (Battle.defender this))
          |> doTick |> blah1 |> blah2
  in this {
    attacker = attacker,
    defender = defender,
    timer = Battle.timer this - 10
    }

-- The |> operator lets us send a piecce of data through a function
-- pipeline.
--
infixl 5 |>
(|>) :: a -> (a -> b) -> b
val |> func = func val
