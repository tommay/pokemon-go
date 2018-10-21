module Battle (
  Battle,
  Battle.init,
  doBattle,
  doBattleOnly,
  attacker,
  defender,
  dps,
  damageInflicted,
  secondsElapsed,
  getDamage
) where

import qualified Attacker
import           Attacker (Attacker)
import qualified Defender
import           Defender (Defender)
import qualified Log
import           Log (Log (Log))
import qualified Logger
import           Logger (Logger)
import qualified Move
import           Move (Move)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified Type
import           Type (Type)
import           WeatherBonus (WeatherBonus)

import qualified Control.Monad.Loops as Loops
import qualified Data.List as List
import qualified Text.Printf as Printf
import qualified Text.Show.Functions

import qualified Debug as D

data Battle = Battle {
  attacker :: Attacker,
  defender :: Defender,
  weatherBonus :: WeatherBonus,
  timer :: Int,
  initialDefenderHp :: Int,
  raidGroup :: Bool
} deriving Show

battleDuration = 100 * 1000

init :: WeatherBonus -> Bool -> Pokemon -> Pokemon -> Battle
init weatherBonus raidGroup attacker defender =
  Battle {
    attacker = Attacker.init attacker,
    defender = Defender.init defender,
    weatherBonus = weatherBonus,
    timer = battleDuration,
    initialDefenderHp = Pokemon.hp defender * 2,
    raidGroup = raidGroup
    }

-- Given an initial Battle state, run the Battle and return the final
-- state when Battle.attackerFainted is true, in a Logger monad with
-- the battle Log.
--
runBattle :: Battle -> Logger (Log Battle) Battle
runBattle =
  Loops.iterateUntilM Battle.attackerFainted Battle.tick

doBattle :: WeatherBonus -> Bool -> Pokemon -> Pokemon ->
  Logger (Log Battle) Battle
doBattle weatherBonus raidGroup attacker defender =
  Battle.runBattle $ Battle.init weatherBonus raidGroup attacker defender

-- Like doBattle but returns only the final Battle state.
--
doBattleOnly :: WeatherBonus -> Bool -> Pokemon -> Pokemon -> Battle
doBattleOnly weatherBonus raidGroup attacker defender =
  fst $ Logger.runLogger $ doBattle weatherBonus raidGroup attacker defender

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

tick :: Battle -> Logger (Log Battle) Battle
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
  battle <- makeLogs $ Battle.updateAttacker battle $ Attacker.makeMove
  battle <- makeLogs $ Battle.updateDefender battle $
    Defender.makeMove (Battle.raidGroup this)
  return battle

checkAttackerHits :: Battle -> Logger (Log Battle) Battle
checkAttackerHits this =
  let attacker = Battle.attacker this
      defender = Battle.defender this
      move = Attacker.move attacker
      damage = getDamage (weatherBonus this)
        (Attacker.pokemon attacker) move (Defender.pokemon defender)
      maybeEnergy = getEnergy move
      battle = this
  in if Attacker.damageWindow attacker == 0 then do
       battle <- makeLogs $
         case maybeEnergy of
           Just energy -> do
             Logger.log $ Printf.printf "Attacker uses %d for %s"
               energy (Move.name move)
             Battle.updateAttacker battle $ Attacker.useEnergy energy
           Nothing -> return battle
       battle <- makeLogs $ do
         Logger.log $ Printf.printf "Defender takes %d from %s"
           damage (Move.name move)
         Battle.updateDefender battle $ Defender.takeDamage damage (Move.isQuick move)
       return battle
     else return battle

checkDefenderHits :: Battle -> Logger (Log Battle) Battle
checkDefenderHits this =
  let defender = Battle.defender this
      attacker = Battle.attacker this
      move = Defender.move defender
      damage = getDamage (weatherBonus this)
        (Defender.pokemon defender) move (Attacker.pokemon attacker)
      maybeEnergy = getEnergy move
      battle = this
  in if Defender.damageWindow defender == 0 then do
       battle <- makeLogs $
         case maybeEnergy of
           Just energy -> do
             Logger.log $ Printf.printf "Defender uses %d for %s"
               energy (Move.name move)
             Battle.updateDefender battle $ Defender.useEnergy energy
           Nothing -> return battle
       battle <- makeLogs $ do
         Logger.log $ Printf.printf "Attacker takes %d from %s"
           damage (Move.name move)
         Battle.updateAttacker battle $ Attacker.takeDamage damage (Move.isQuick move)
       return battle
     else return battle

updateAttacker :: Battle -> (Attacker -> Logger String Attacker) -> Logger String Battle
updateAttacker this fn = do
  attacker <- fn $ Battle.attacker this
  return $ this { attacker = attacker }

updateDefender :: Battle -> (Defender -> Logger String Defender) -> Logger String Battle
updateDefender this fn = do
  defender <- fn $ Battle.defender this
  return $ this { defender = defender }

getDamage :: WeatherBonus -> Pokemon -> Move -> Pokemon -> Int
getDamage weatherBonus attacker move defender =
  let power = Move.power move
      stab = Move.stabFor move $ Pokemon.types attacker
      weather = weatherBonus $ Move.moveType move
      effectiveness = Move.effectivenessAgainst move $ Pokemon.types defender
      attack = Pokemon.attack attacker
      defense = Pokemon.defense defender
  in floor $ power * stab * weather * effectiveness * attack / defense / 2 + 1

getEnergy :: Move -> Maybe Int
getEnergy move =
  if Move.isCharge move
    then Just $ negate $ Move.energy move
    else Nothing

makeLogs :: Logger String Battle -> Logger (Log Battle) Battle
makeLogs battleLogger =
  let makeLog battle string = Log {
        Log.state = battle,
        Log.what = string
        }
  in decorateLog makeLog battleLogger

decorateLog :: (a -> w -> u) -> Logger w a -> Logger u a
decorateLog f m =
  let (a, ws) = Logger.runLogger m
      us = map (f a) ws
  in Logger.logger (a, us)
