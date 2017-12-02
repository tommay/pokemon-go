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
) where

import           Action (Action (Action))
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified Move
import           Move (Move)

import qualified Control.Monad.Writer as Writer
import           Control.Monad.Writer (Writer)
import qualified Text.Printf as Printf

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

tick :: Attacker -> Attacker
tick this =
  this {
    cooldown = Attacker.cooldown this - 10,
    damageWindow = Attacker.damageWindow this - 10
    }

makeMove :: Attacker -> Attacker
makeMove this =
  if Attacker.cooldown this == 0
    then makeMove' this
    else this

makeMove' :: Attacker -> Attacker
makeMove' this =
  let quick = Attacker.quick this
      charge = Attacker.charge this
      move':moves' = case Attacker.moves this of
        [] ->
          if Attacker.energy this >= negate (Move.energy charge)
            -- Do an extra quick move to simulate delayed player reaction
            -- to the flashing charge bars.
            then [quick, charge]
            else [quick]
        val -> val
      -- If it's a quick move, its energy is available immediately.
      -- Charge move energy is subtracted at damageWindowStart.
      energy' = if Move.isQuick move'
        then minimum [100, Attacker.energy this + Move.energy move']
        else Attacker.energy this
      cooldown' = Move.durationMs move'
      -- Set countdown until damage is done to the opponent and its
      -- energy increases and our energy decreases.
      damageWindow' = Move.damageWindow move'
  in this {
       energy = energy',
       cooldown = cooldown',
       moves = moves',
       move = move',
       damageWindow = damageWindow'
       }

takeDamage :: Pokemon -> Move -> Attacker -> Writer [Action] Attacker
takeDamage pokemon move this = do
  let damageDone = damage move pokemon (Attacker.pokemon this)
      format = Printf.printf "Attacker takes %d damage: hp = %d, energy = %d"
      result = this {
        hp = Attacker.hp this - damageDone,
        energy = minimum [100, Attacker.energy this + (damageDone + 1) `div` 2]
        }
  Writer.tell [Action (format damageDone (Attacker.hp result)
    (Attacker.energy result))]
  return result

useEnergy :: Attacker -> Attacker
useEnergy this =
  let move = Attacker.move this
  in if Move.isCharge move
       then this {
         energy = Attacker.energy this + Move.energy move
         }
       else this

damage :: Move -> Pokemon -> Pokemon -> Int
damage move attacker defender =
  let power = Move.power move
      stab = Move.stabFor move $ Pokemon.types attacker
      effectiveness = Move.effectivenessAgainst move $ Pokemon.types defender
      attack = Pokemon.attack attacker
      defense = Pokemon.defense defender
  in floor $ power * stab * effectiveness * attack / defense / 2 + 1
