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
import qualified Move
import           Move (Move)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified Type
import           Type (Type)

import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.Writer as Writer
import           Control.Monad.Writer (Writer)
import qualified Data.List as List
import qualified Data.Tuple.Sequence as Tuple
import qualified System.Random as Random
import qualified Text.Printf as Printf

import qualified Debug as D

data Battle = Battle {
  attacker :: Attacker,
  defender :: Defender,
  weatherBonus :: Type -> Float,
  timer :: Int,
  initialDefenderHp :: Int
}

battleDuration = 100 * 1000

init :: (Type -> Float) -> Pokemon -> Pokemon -> Battle
init weatherBonus attacker defender =
  let rnd = Random.mkStdGen 23
  in Battle {
       attacker = Attacker.init attacker,
       defender = Defender.init rnd defender,
       weatherBonus = weatherBonus,
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

tell :: a -> Writer [a] ()
tell a =
  Writer.tell [a]

tick :: Battle -> Writer [Action] Battle
tick this = do
  let blah1 :: (Attacker, Defender) -> Writer [Action] (Attacker, Defender)
      blah1 (attacker, defender) =
        let damage = getDamage
              (weatherBonus this)
              (Defender.pokemon defender)
              (Defender.move defender)
              (Attacker.pokemon attacker)
        in if Defender.damageWindow defender == 0
          then Tuple.sequenceT (
            addOuch $ do
              tell $ Printf.printf "Attacker takes %d from %s"
                damage (Move.name $ Defender.move defender)
              return $ Attacker.takeDamage attacker damage,
            Defender.useEnergy defender)
          else return (attacker, defender)
      blah2 :: (Attacker, Defender) -> Writer [Action] (Attacker, Defender)
      blah2 (attacker, defender) =
        let damage = getDamage
              (weatherBonus this)
              (Attacker.pokemon attacker)
              (Attacker.move attacker)
              (Defender.pokemon defender)
        in if Attacker.damageWindow attacker == 0
          then Tuple.sequenceT (
            Attacker.useEnergy attacker,
            addOuch $ do
              tell $ Printf.printf "Defender takes %d from %s"
                damage (Move.name $ Attacker.move attacker)
              return $ Defender.takeDamage defender damage)
          else return (attacker, defender)
      doTick ticks (attacker, defender) =
        return $ (Attacker.tick ticks attacker, Defender.tick ticks defender)
      makeMove (attacker, defender) =
        Tuple.sequenceT (Attacker.makeMove attacker, Defender.makeMove defender)
      attacker = Battle.attacker this
      defender =  Battle.defender this
      pair = (attacker, defender)
  let ticks = minimum $
        [Attacker.nextTick attacker, Defender.nextTick defender]
  pair <- doTick ticks pair
  pair <- blah1 pair
  pair <- blah2 pair
  pair <- makeMove pair
  let (attacker, defender) = pair
  return $ this {
    attacker = attacker,
    defender = defender,
    timer = Battle.timer this - ticks
    }

getDamage :: (Type -> Float) -> Pokemon -> Move -> Pokemon -> Int
getDamage weatherBonus attacker move defender =
  let power = Move.power move
      stab = Move.stabFor move $ Pokemon.types attacker
      weather = weatherBonus $ Move.moveType move
      effectiveness = Move.effectivenessAgainst move $ Pokemon.types defender
      attack = Pokemon.attack attacker
      defense = Pokemon.defense defender
  in floor $ power * stab * weather *effectiveness * attack / defense / 2 + 1
