module Attacker (
  Attacker,
  Attacker.init,
  pokemon,
  hp,
  energy,
  move,
  damageWindow,
  tick,
  takeDamage,
  useEnergy,
  makeMove,
  fainted,
  nextTick,
) where

import qualified Logger
import           Logger (Logger)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified Move
import           Move (Move)

import qualified Text.Printf as Printf

import Debug as D

data Attacker = Attacker {
  pokemon :: Pokemon,
  hp :: Int,
  energy :: Int,
  quickEnergy :: Int,
  damageEnergy :: Int,
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
       quickEnergy = 0,
       damageEnergy = 0,
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

makeMove :: Attacker -> Logger String Attacker
makeMove this =
  if Attacker.cooldown this == 0
    then makeMove' this
    else return this

makeMove' :: Attacker -> Logger String Attacker
makeMove' this = do
  let quick = Attacker.quick this
      charge = Attacker.charge this
  move':moves' <- case Attacker.moves this of
    [] ->
      if Attacker.energy this >= negate (Move.energy charge)
        -- Do an extra quick move to simulate delayed player reaction
        -- to the flashing charge bars.
        then do
          Logger.log $ "Attacker can use " ++ Move.name charge
          return [quick, charge]
        else return [quick]
    val -> return val
  let -- If it's a quick move, its energy is available immediately.
      -- Charge move energy is subtracted at damageWindowStart.
      energy' = if Move.isQuick move'
        then minimum [100, Attacker.energy this + Move.energy move']
        else Attacker.energy this
      quickEnergy' =
        Attacker.quickEnergy this + (energy' - Attacker.energy this)
      cooldown' = Move.durationMs move'
      -- Set countdown until damage is done to the opponent and its
      -- energy increases and our energy decreases.
      damageWindow' = Move.damageWindow move'
      result = this {
        energy = energy',
        quickEnergy = quickEnergy',
        cooldown = cooldown',
        moves = moves',
        move = move',
        damageWindow = damageWindow'
        }
  Logger.log $ "Attacker uses " ++ (Move.name $ Attacker.move result)
  return result

takeDamage :: Int -> Attacker -> Logger String Attacker
takeDamage damage this =
  let energy' = minimum [100, Attacker.energy this + (damage + 1) `div` 2]
  in return $ this {
       hp = Attacker.hp this - damage,
       energy = energy',
       damageEnergy =
         Attacker.damageEnergy this + (energy' - Attacker.energy this)
       }

useEnergy :: Int -> Attacker -> Logger String Attacker
useEnergy energy this =
  return $ this {
    energy = Attacker.energy this - energy
    }
