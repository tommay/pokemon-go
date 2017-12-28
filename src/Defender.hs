module Defender (
  Defender,
  Defender.init,
  pokemon,
  hp,
  energy,
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

import qualified System.Random as Random
import qualified Text.Printf as Printf

import qualified Debug as D

data Defender = Defender {
  pokemon :: Pokemon,
  hp :: Int,
  energy :: Int,
  quickEnergy :: Int,
  damageEnergy :: Int,
  cooldown :: Int,        -- time until the next move.
  moves :: [(Move, Int)], -- next move(s) to do.
  move :: Move,           -- move in progess.
  damageWindow :: Int,
  rnd :: Random.StdGen
} deriving (Show)

init :: Random.StdGen -> Pokemon -> Defender
init rnd pokemon =
  let quick = Pokemon.quick pokemon
  in Defender {
       pokemon = pokemon,
       hp = Pokemon.hp pokemon * 2,
       energy = 0,
       quickEnergy = 0,
       damageEnergy = 0,
       cooldown = 1600,
       -- The first two moves are always quick and the interval is fixed.
       -- https://thesilphroad.com/tips-and-news/defender-attacks-twice-immediately
       -- XXX What if the damage window for the first move is never reached?
       -- Is it also fixed?
       moves = [(quick, 1000),
                (quick, Move.durationMs quick + 2000)],
       move = quick,  -- Not used.
       damageWindow = -1,
       rnd = rnd
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

makeMove :: Defender -> Logger String Defender
makeMove this =
  if Defender.cooldown this == 0
    then makeMove' this
    else return this

makeMove' :: Defender -> Logger String Defender
makeMove' this = do
  let -- Get the next move and any move(s) after that.
      (move', cooldown'):moves' = Defender.moves this
  Logger.log $ "Defender uses " ++ Move.name move'
  let -- If it's a quick move, its energy is available immediately
      -- for the decision about the next move.  Charge move energy
      -- is subtracted at damageWindowStart; this affects energy
      -- damage until it is subtracted.
      energy' = if Move.isQuick move'
        then minimum [100, Defender.energy this + Move.energy move']
        else Defender.energy this
      quickEnergy' =
        Defender.quickEnergy this + (energy' - Defender.energy this)
      -- But we have to account for the charge energy that will be
      -- used when deciding what move to make.
      decisionEnergy = if Move.isQuick move'
        then energy'
        else energy' + Move.energy move'  -- Charge Move.energy is negative.
      -- Figure out our next move.
      quick = Defender.quick this
      charge = Defender.charge this
      (random, rnd') = Random.random $ Defender.rnd this
  moves'' <- case moves' of
    [] -> do
      -- Both quick moves and charge moves get an additional 1.5-2.5
      -- seconds added to their duration.  Just use the average, 2.
      if decisionEnergy >= negate (Move.energy charge) then do
          Logger.log $ "Defender can use " ++ Move.name charge
          if (random :: Float) < 0.5 then do
            Logger.log $ "Defender chooses " ++ Move.name charge ++ " for next move"
            return [(charge, Move.durationMs charge + 2000)]
          else do
            Logger.log $ "Defender chooses " ++ Move.name quick ++ " for next move"
            return [(quick, Move.durationMs quick + 2000)]
      else do
        Logger.log $ "Defender must use " ++ Move.name quick ++ " for next move"
        return [(quick, Move.durationMs quick + 2000)]
    val -> return val
  -- Set countdown until damage is done to the opponent and it gets
  -- its energy boost and our charge move energy is subtracted.
  let damageWindow' = Move.damageWindow move'
  return $ this {
    energy = energy',
    quickEnergy = quickEnergy',
    cooldown = cooldown',
    moves = moves'',
    move = move',
    damageWindow = damageWindow',
    rnd = rnd'
    }

takeDamage :: Int -> Defender -> Logger String Defender
takeDamage damage this =
  let energy' = minimum [100, Defender.energy this + (damage + 1) `div` 2]
  in return $ this {
       hp = Defender.hp this - damage,
       energy = energy',
       damageEnergy =
         Defender.damageEnergy this + (energy' - Defender.energy this)
       }

useEnergy :: Int -> Defender -> Logger String Defender
useEnergy energy this =
  return $ this {
    energy = Defender.energy this - energy
    }
