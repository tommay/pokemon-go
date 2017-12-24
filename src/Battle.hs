module Battle (
  Battle,
  Battle.init,
  runBattle,
  runBattleOnly,
  attacker,
  defender,
  dps,
  damageInflicted,
  secondsElapsed,
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
runBattle :: Battle -> Writer [Action Battle] Battle
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

tick :: Battle -> Writer [Action Battle] Battle
tick this = do
  let attacker = Battle.attacker this
      defender = Battle.defender this
      ticks = minimum $
        [Attacker.nextTick attacker, Defender.nextTick defender]
      battle =
        this {
          attacker = Attacker.tick ticks attacker,
          defender = Defender.tick ticks defender,
          timer = Battle.timer this - ticks
        }
  battle <- checkAttackerHits battle
  battle <- checkDefenderHits battle
  battle <- makeActions $ Battle.updateAttacker battle $ Attacker.makeMove
  battle <- makeActions $ Battle.updateDefender battle $ Defender.makeMove
  return battle

checkAttackerHits :: Battle -> Writer [Action Battle] Battle
checkAttackerHits this =
  let attacker = Battle.attacker this
      defender = Battle.defender this
      move = Attacker.move attacker
      damage = getDamage (weatherBonus this)
        (Attacker.pokemon attacker) move (Defender.pokemon defender)
      maybeEnergy = getEnergy move
      battle = this
  in if Attacker.damageWindow attacker == 0 then do
       battle <- makeActions $
         case maybeEnergy of
           Just energy -> do
             Writer.tell [Printf.printf "Attacker uses %d for %s"
               energy (Move.name move)]
             Battle.updateAttacker battle $ Attacker.useEnergy energy
           Nothing -> return battle
       battle <- makeActions $ do
         Writer.tell [Printf.printf "Defender takes %d from %s"
           damage (Move.name move)]
         Battle.updateDefender battle $ Defender.takeDamage damage
       return battle
     else return battle

checkDefenderHits :: Battle -> Writer [Action Battle] Battle
checkDefenderHits this =
  let defender = Battle.defender this
      attacker = Battle.attacker this
      move = Defender.move defender
      damage = getDamage (weatherBonus this)
        (Defender.pokemon defender) move (Attacker.pokemon attacker)
      maybeEnergy = getEnergy move
      battle = this
  in if Defender.damageWindow defender == 0 then do
       battle <- makeActions $
         case maybeEnergy of
           Just energy -> do
             Writer.tell [Printf.printf "Defender uses %d for %s"
               energy (Move.name move)]
             Battle.updateDefender battle $ Defender.useEnergy energy
           Nothing -> return battle
       battle <- makeActions $ do
         Writer.tell [Printf.printf "Attacker takes %d from %s"
           damage (Move.name move)]
         Battle.updateAttacker battle $ Attacker.takeDamage damage
       return battle
     else return battle

updateAttacker :: Battle -> (Attacker -> Writer [String] Attacker) -> Writer [String] Battle
updateAttacker this fn = do
  attacker <- fn $ Battle.attacker this
  return $ this { attacker = attacker }

updateDefender :: Battle -> (Defender -> Writer [String] Defender) -> Writer [String] Battle
updateDefender this fn = do
  defender <- fn $ Battle.defender this
  return $ this { defender = defender }

getDamage :: (Type -> Float) -> Pokemon -> Move -> Pokemon -> Int
getDamage weatherBonus attacker move defender =
  let power = Move.power move
      stab = Move.stabFor move $ Pokemon.types attacker
      weather = weatherBonus $ Move.moveType move
      effectiveness = Move.effectivenessAgainst move $ Pokemon.types defender
      attack = Pokemon.attack attacker
      defense = Pokemon.defense defender
  in floor $ power * stab * weather *effectiveness * attack / defense / 2 + 1

getEnergy :: Move -> Maybe Int
getEnergy move =
  if Move.isCharge move
    then Just $ negate $ Move.energy move
    else Nothing

makeActions :: Writer [String] Battle -> Writer [Action Battle] Battle
makeActions battleWriter =
  let makeAction battle string = Action {
        Action.state = battle,
        Action.what = string
        }
  in decorateLog makeAction battleWriter

decorateLog :: (a -> w -> u) -> Writer [w] a -> Writer [u] a
decorateLog f m =
  let (a, ws) = Writer.runWriter m
      us = map (f a) ws
  in Writer.writer (a, us)
