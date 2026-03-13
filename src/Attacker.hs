module Attacker (
  Attacker,
  Attacker.init,
  pokemon,
  hp,
  fastDamage,
  chargedDamage,
  energy,
  fastEnergy,
  damageEnergy,
  wastedEnergy,
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

-- Battle mechanics are from
-- https://pokemongo.gamepress.gg/new-discoveries-theory-battle-mechanics
-- See also Battle.hs and Defender.hs.

data Attacker = Attacker {
  pokemon :: Pokemon,
  hp :: Int,
  fastDamage :: Int,
  chargedDamage :: Int,
  energy :: Int,
  fastEnergy :: Int,
  damageEnergy :: Int,
  wastedEnergy :: Int,
  cooldown :: Int,        -- time until the next move.
  moves :: [Move],        -- next move(s) to do.
  move :: Move,           -- move in progess.
  damageWindow :: Int
} deriving (Show)

init :: Pokemon -> Attacker
init pokemon =
  let fast = Pokemon.fast pokemon
  in Attacker {
       pokemon = pokemon,
       hp = Pokemon.hp pokemon,
       fastDamage = 0,
       chargedDamage = 0,
       energy = 0,
       fastEnergy = 0,
       damageEnergy = 0,
       wastedEnergy = 0,
       cooldown = 700,
       moves = [],
       move = fast,  -- Not used.
       damageWindow = -1
       }

fainted :: Attacker -> Bool
fainted this =
  Attacker.hp this <= 0

fast :: Attacker -> Move
fast this =
  Pokemon.fast $ Attacker.pokemon this

charged :: Attacker -> Move
charged this =
  Pokemon.charged $ Attacker.pokemon this

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
  let fast = Attacker.fast this
      charged = Attacker.charged this
  ~(move':moves') <- case Attacker.moves this of
    [] ->
      if Attacker.energy this >= negate (Move.energy charged)
{-
        -- Do an extra fast move to simulate delayed player reaction
        -- to the flashing charge bars.
        then do
          Logger.log $ "Attacker can use " ++ Move.name charged
          return [fast, charged]
-}
        -- Now  that the battle gui has changed to enable the charged move
        -- button whenever it's ready and the player can keep tapping in the
        -- same place, there is no reaction time to simulate and we can
        -- fire fire off the charged move asap.
        then do
          Logger.log $ "Attacker can use " ++ Move.name charged
          return [charged]
        else return [fast]
    val -> return val
  let -- If it's a fast move, its energy is available immediately.
      -- Charged move energy is subtracted at damageWindowStart.
      (okEnergy, wastedEnergy) = if Move.isFast move'
        then calcAllowedEnergy (Move.energy move') this
        else (0, 0)
      cooldown' = Move.durationMs move'
      -- Set countdown until damage is done to the opponent and its
      -- energy increases and our energy decreases.
      damageWindow' = Move.damageWindow move'
      result = this {
        energy = Attacker.energy this + okEnergy,
        fastEnergy = Attacker.fastEnergy this + okEnergy,
        wastedEnergy = Attacker.wastedEnergy this + wastedEnergy,
        cooldown = cooldown',
        moves = moves',
        move = move',
        damageWindow = damageWindow'
        }
  Logger.log $ "Attacker uses " ++ (Move.name $ Attacker.move result)
  return result

takeDamage :: Int -> Bool -> Attacker -> Logger String Attacker
takeDamage damage isFast this =
  let (okEnergy, wastedEnergy) = calcAllowedEnergy ((damage + 1) `div` 2) this
  in return $ this {
       hp = Attacker.hp this - damage,
       fastDamage = Attacker.fastDamage this + if isFast then damage else 0,
       chargedDamage = Attacker.chargedDamage this + if isFast then 0 else damage,
       energy = Attacker.energy this + okEnergy,
       damageEnergy = Attacker.damageEnergy this + okEnergy,
       wastedEnergy = Attacker.wastedEnergy this + wastedEnergy
       }

calcAllowedEnergy :: Int -> Attacker -> (Int, Int)
calcAllowedEnergy energy this =
  let newEnergy = Attacker.energy this + energy
      wastedEnergy = newEnergy - 100
  in if wastedEnergy <= 0
       then (energy, 0)
       else (energy - wastedEnergy, wastedEnergy)

useEnergy :: Int -> Attacker -> Logger String Attacker
useEnergy energy this =
  return $ this {
    energy = Attacker.energy this - energy
    }
