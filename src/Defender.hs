module Defender (
  Defender,
  Defender.init,
  pokemon,
  hp,
  quickDamage,
  chargeDamage,
  energy,
  quickEnergy,
  damageEnergy,
  wastedEnergy,
  move,
  damageWindow,
  tick,
  takeDamage,
  useEnergy,
  makeMove,
  nextTick,
) where

import qualified Logger
import           Logger (Logger)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified Move
import           Move (Move)

import qualified Text.Printf as Printf

import qualified Debug as D

-- Battle mechanics are from
-- https://pokemongo.gamepress.gg/new-discoveries-theory-battle-mechanics
-- See also Battle.hs and Attacker.hs.

data Defender = Defender {
  pokemon :: Pokemon,
  hp :: Int,
  quickDamage :: Int,
  chargeDamage :: Int,
  energy :: Int,
  quickEnergy :: Int,
  damageEnergy :: Int,
  wastedEnergy :: Int,
  cooldown :: Int,        -- time until the next move.
  moves :: [(Move, Int)], -- next move(s) to do.
  move :: Move,           -- move in progess.
  damageWindow :: Int,
  rnd :: [Bool]
} deriving (Show)

init :: Pokemon -> Defender
init pokemon =
  let quick = Pokemon.quick pokemon
  in Defender {
       pokemon = pokemon,
       hp = Pokemon.hp pokemon * 2,
       quickDamage = 0,
       chargeDamage = 0,
       energy = 0,
       quickEnergy = 0,
       damageEnergy = 0,
       wastedEnergy = 0,
       cooldown = 1600,
       -- The first two moves are always quick and the interval is fixed.
       -- https://thesilphroad.com/tips-and-news/defender-attacks-twice-immediately
       -- XXX What if the damage window for the first move is never reached?
       -- Is it also fixed?
       moves = [(quick, 1000),
                (quick, Move.durationMs quick + 2000)],
       move = quick,  -- Not used.
       damageWindow = -1,
       rnd = cycle [True, False]
       }

quick :: Defender -> Move
quick this =
  Pokemon.quick $ Defender.pokemon this

charge :: Defender -> Move
charge this =
  Pokemon.charge $ Defender.pokemon this

tick :: Int -> Defender -> Defender
tick n this =
  this {
    cooldown = Defender.cooldown this - n,
    damageWindow = Defender.damageWindow this - n
    }

nextTick :: Defender -> Int
nextTick this =
  minimum $ filter (> 0) [Defender.cooldown this, Defender.damageWindow this]

makeMove :: Bool -> Defender -> Logger String Defender
makeMove raidGroup this =
  if Defender.cooldown this == 0
    then makeMove' raidGroup this
    else return this

makeMove' :: Bool -> Defender -> Logger String Defender
makeMove' raidGroup this = do
  let -- Get the next move and any move(s) after that.
      (move', cooldown'):moves' = Defender.moves this
  Logger.log $ "Defender uses " ++ Move.name move'
  let -- If it's a quick move, its energy is available immediately
      -- for the decision about the next move.  Charge move energy
      -- is subtracted at damageWindowStart; this affects damage energy
      -- gain until it is subtracted  But we have to account for the
      -- charge energy that will be used to decide what move to make.
      (okEnergy, wastedEnergy, newEnergy, decisionEnergy) =
        if Move.isQuick move' then
          let (okEnergy, wastedEnergy) =
                calcAllowedEnergy (Move.energy move') this
          in (okEnergy, wastedEnergy,
               Defender.energy this + okEnergy,
               Defender.energy this + okEnergy)
        else
          (0, 0,
            Defender.energy this,
            -- Charge Move.energy is negative.
            Defender.energy this + Move.energy move')
      quick = Defender.quick this
      charge = Defender.charge this
  -- Be careful to advance the random number generator only if we actually
  -- need to use it, since we're relying on the True, False, ... sequence.
  (moves'', rnd') <- case moves' of
    [] -> do
      -- Both quick moves and charge moves get an additional 1.5-2.5
      -- seconds added to their duration.  Just use the average, 2.
      -- https://www.reddit.com/r/TheSilphRoad/comments/52b453/testing_gym_combat_misconceptions_2/
      if raidGroup || decisionEnergy >= negate (Move.energy charge) then do
        Logger.log $ "Defender can use " ++ Move.name charge
        let (random : rnd') = Defender.rnd this
        if random then do
          Logger.log $ "Defender chooses " ++ Move.name charge ++ " for next move"
          return ([(charge, Move.durationMs charge + 2000)], rnd')
        else do
          Logger.log $ "Defender chooses " ++ Move.name quick ++ " for next move"
          return ([(quick, Move.durationMs quick + 2000)], rnd')
      else do
        Logger.log $ "Defender must use " ++ Move.name quick ++ " for next move"
        return ([(quick, Move.durationMs quick + 2000)], Defender.rnd this)
    val -> return (val, Defender.rnd this)
  -- Set countdown until damage is done to the opponent and it gets
  -- its energy boost and our charge move energy is subtracted.
  let damageWindow' = Move.damageWindow move'
  return $ this {
    energy = newEnergy,
    quickEnergy = Defender.quickEnergy this + okEnergy,
    wastedEnergy = Defender.wastedEnergy this + wastedEnergy,
    cooldown = cooldown',
    moves = moves'',
    move = move',
    damageWindow = damageWindow',
    rnd = rnd'
    }

takeDamage :: Int -> Bool -> Defender -> Logger String Defender
takeDamage damage isQuick this =
  let (okEnergy, wastedEnergy) = calcAllowedEnergy ((damage + 1) `div` 2) this
  in return $ this {
       hp = Defender.hp this - damage,
       quickDamage = Defender.quickDamage this + if isQuick then damage else 0,
       chargeDamage = Defender.chargeDamage this + if isQuick then 0 else damage,
       energy = Defender.energy this + okEnergy,
       damageEnergy = Defender.damageEnergy this + okEnergy,
       wastedEnergy = Defender.wastedEnergy this + wastedEnergy
       }

calcAllowedEnergy :: Int -> Defender -> (Int, Int)
calcAllowedEnergy energy this =
  let newEnergy = Defender.energy this + energy
      wastedEnergy = newEnergy - 100
  in if wastedEnergy <= 0
       then (energy, 0)
       else (energy - wastedEnergy, wastedEnergy)

useEnergy :: Int -> Defender -> Logger String Defender
useEnergy energy this =
  return $ this {
    energy = Defender.energy this - energy
    }
