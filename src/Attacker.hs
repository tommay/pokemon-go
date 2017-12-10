module Attacker (
  Attacker,
  pokemon,
  move,
  damageWindow,
  Attacker.init,
  tick,
  takeDamage,
  useEnergy,
  makeMove,
  fainted,
  nextTick,
) where

import           Action (Action (Action))
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified Move
import           Move (Move)

import qualified Control.Monad.Writer as Writer
import           Control.Monad.Writer (Writer)
import qualified Text.Printf as Printf

import Debug as D

data Attacker = Attacker {
  pokemon :: Pokemon,
  hp :: Int,
  energy :: Int,
  cooldown :: Int,        -- time until the next move.
  moves :: [Move],        -- next move(s) to do.
  move :: Move,           -- move in progess.
  damageWindow :: Int
} deriving (Show)

init :: Pokemon -> Attacker
init pokemon =
  let quick = Pokemon.quick pokemon
  in Attacker {
       pokemon = pokemon,
       hp = Pokemon.hp pokemon,
       energy = 0,
       cooldown = 700,
       moves = [],
       move = quick,  -- Not used.
       damageWindow = -1
       }

fainted :: Attacker -> Bool
fainted this =
  Attacker.hp this <= 0

quick :: Attacker -> Move
quick this =
  Pokemon.quick $ Attacker.pokemon this

charge :: Attacker -> Move
charge this =
  Pokemon.charge $ Attacker.pokemon this

tick :: Int -> Attacker -> Attacker
tick n this =
  this {
    cooldown = Attacker.cooldown this - n,
    damageWindow = Attacker.damageWindow this - n
    }

nextTick :: Attacker -> Int
nextTick this =
  minimum $ filter (> 0) [Attacker.cooldown this, Attacker.damageWindow this]

makeMove :: Attacker -> Writer [Action] Attacker
makeMove this =
  if Attacker.cooldown this == 0
    then makeMove' this
    else return this

makeMove' :: Attacker -> Writer [Action] Attacker
makeMove' this = do
  let quick = Attacker.quick this
      charge = Attacker.charge this
  move':moves' <- case Attacker.moves this of
    [] ->
      if Attacker.energy this >= negate (Move.energy charge)
        -- Do an extra quick move to simulate delayed player reaction
        -- to the flashing charge bars.
        then do
          Writer.tell [Action ("Attacker can use " ++ Move.name charge)]
          return [quick, charge]
        else return [quick]
    val -> return val
  let -- If it's a quick move, its energy is available immediately.
      -- Charge move energy is subtracted at damageWindowStart.
      energy' = if Move.isQuick move'
        then minimum [100, Attacker.energy this + Move.energy move']
        else Attacker.energy this
      cooldown' = Move.durationMs move'
      -- Set countdown until damage is done to the opponent and its
      -- energy increases and our energy decreases.
      damageWindow' = Move.damageWindow move'
  Writer.tell [Action ("Attacker uses " ++ (Move.name move'))]
  return $ this {
    energy = energy',
    cooldown = cooldown',
    moves = moves',
    move = move',
    damageWindow = damageWindow'
    }

takeDamage :: Pokemon -> Move -> Attacker -> Writer [String] Attacker
takeDamage pokemon move this = do
  let damageDone = damage move pokemon (Attacker.pokemon this)
      format = Printf.printf "Attacker takes %d damage from %s: hp = %d, energy = %d"
      result = this {
        hp = Attacker.hp this - damageDone,
        energy = minimum [100, Attacker.energy this + (damageDone + 1) `div` 2]
        }
  Writer.tell $ [format damageDone (Move.name move) (Attacker.hp result)
    (Attacker.energy result)]
  return result

useEnergy :: Attacker -> Writer [Action] Attacker
useEnergy this = do
  let move = Attacker.move this
      energyUsed = negate $ Move.energy move
      result = this {
        energy = Attacker.energy this - energyUsed
        }
      format = Printf.printf "Attacker uses %d energy: hp = %d, energy = %d"
  if Move.isCharge move
    then do
      Writer.tell [Action (format energyUsed (Attacker.hp result)
         (Attacker.energy result))]
      return result
    else return this

damage :: Move -> Pokemon -> Pokemon -> Int
damage move attacker defender =
  let power = Move.power move
      stab = Move.stabFor move $ Pokemon.types attacker
      effectiveness = Move.effectivenessAgainst move $ Pokemon.types defender
      attack = Pokemon.attack attacker
      defense = Pokemon.defense defender
  in floor $ power * stab * effectiveness * attack / defense / 2 + 1
