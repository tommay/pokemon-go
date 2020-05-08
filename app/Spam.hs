{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import           Move (Move)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Type
import           Type (Type)

import qualified Debug

import           Control.Monad (join, forM, forM_)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  attacker :: String,
  defender :: Maybe String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optAttacker <*> optDefender
      optAttacker = O.strArgument (O.metavar "ATTACKER")
      optDefender = O.optional $ O.strArgument (O.metavar "DEFENDER")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Rate movesets by spamminess.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      base <- GameMaster.getPokemonBase gameMaster $ attacker options
      maybeDefenderBase <- do
        case defender options of
          Just species -> Just <$> GameMaster.getPokemonBase gameMaster species
          Nothing -> return Nothing
      let types = PokemonBase.types base
          defenderTypes = case maybeDefenderBase of
            Just defenderBase -> PokemonBase.types defenderBase
            Nothing -> []
          moveSets = [(fast, charged) |
            fast <- PokemonBase.quickMoves base,
            charged <- PokemonBase.chargeMoves base]
          toName :: Move -> Move -> String
          toName fast charged =
            Printf.printf "%s(%d) / %s(%d)"
              (Move.name fast) (Move.pvpEnergyDelta fast)
              (Move.name charged) (negate $ Move.pvpEnergyDelta charged)
          maxNameLength = maximum $
            map (\ (fast, charged) -> length $ toName fast charged) moveSets
          toStuff :: (Move, Move) -> (String, Int, Int, Float, Float)
          toStuff (fast, charged) =
            let name = toName fast charged
                (turnsPerCycle, fastMovesPerCycle, dpt, dpe) =
                  spam types defenderTypes fast charged
            in (name, turnsPerCycle, fastMovesPerCycle, dpt, dpe)
          stuff = map toStuff moveSets
      forM_ stuff $ \ (name, turnsPerCycle, fastMovesPerCycle, dpt, dpe) ->
        putStrLn $ Printf.printf "%-*s: %d(%d) %.2f %.2f"
          maxNameLength name turnsPerCycle fastMovesPerCycle dpt dpe
    )
    $ Exit.die

spam :: [Type] -> [Type] -> Move -> Move -> (Int, Int, Float, Float)
spam types defenderTypes fast charged =
  let chargedEnergy = - Move.pvpEnergyDelta charged
      fastEnergy = Move.pvpEnergyDelta fast
      fastMovesPerCycle = (chargedEnergy + fastEnergy - 1) `div` fastEnergy
      turnsPerCycle = fastMovesPerCycle * (Move.pvpDurationTurns fast + 1)
      multiplier move =
        Move.stabFor move types * Move.effectivenessAgainst move defenderTypes
      multiplierFast = multiplier fast
      multiplierCharged = multiplier charged
      damagePerCycle =
        (fromIntegral fastMovesPerCycle) * (Move.pvpPower fast) *
          multiplierFast
        + (Move.pvpPower charged * multiplierCharged)
      dpt = damagePerCycle / fromIntegral turnsPerCycle
      dpe = (Move.pvpPower charged) * multiplierCharged /
        fromIntegral chargedEnergy
  in (turnsPerCycle, fastMovesPerCycle, dpt, dpe)
