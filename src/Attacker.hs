module Attacker (
  Attacker,
  pokemon,
  move,
  damageWindow,
  Attacker.init,
  tick,
  takeDamage,
  useEnergy,
  fainted,
) where

import qualified Pokemon
import           Pokemon (Pokemon)
import qualified Move
import           Move (Move)

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
       moves = [quick],
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
  if Attacker.cooldown this /= 0
    then passTheTime this
    else makeAMove this

passTheTime :: Attacker -> Attacker
passTheTime this =
  this {
    cooldown = Attacker.cooldown this - 10,
    damageWindow = Attacker.damageWindow this - 10
    }

makeAMove :: Attacker -> Attacker
makeAMove this =
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
      -- Now thet we've decided the move we can add its energy.
      energy' = minimum [100, Attacker.energy this + Move.energy move']
      cooldown' = Move.durationMs move'
      -- Set countdown until damage is done to the opponent and its
      -- energy increases.
      damageWindow' = Move.damageWindow move'
  in this {
       energy = energy',
       cooldown = cooldown',
       moves = moves',
       move = move',
       damageWindow = damageWindow'
       }

takeDamage :: Pokemon -> Move -> Attacker -> Attacker
takeDamage pokemon move this =
  let damageDone = damage move pokemon (Attacker.pokemon this)
  in this {
       hp = Attacker.hp this - damageDone,
       energy = minimum [100,
         Attacker.energy this + (damageDone + 1) `div` 2]
       }

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
