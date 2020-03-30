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
  attacker :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optAttacker
      optAttacker = O.strArgument (O.metavar "POKEMON")
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
      let species = attacker options
      base <- GameMaster.getPokemonBase gameMaster species
      let moveSets = [(fast, charged) |
            fast <- PokemonBase.quickMoves base,
            charged <- PokemonBase.chargeMoves base]
          toName :: Move -> Move -> String
          toName fast charged =
            Printf.printf "%s(%d) / %s(%d)"
              (Move.name fast) (Move.pvpEnergyDelta fast)
              (Move.name charged) (negate $ Move.pvpEnergyDelta charged)
          maxNameLength = maximum $
            map (\ (fast, charged) -> length $ toName fast charged) moveSets
          toStuff :: (Move, Move) -> (String, Int, Int, Float)
          toStuff (fast, charged) =
            let name = toName fast charged
                (turnsPerCycle, fastMovesPerCycle, dpt) = spam fast charged
            in (name, turnsPerCycle, fastMovesPerCycle, dpt)
          stuff = map toStuff moveSets
      forM_ stuff $ \ (name, turnsPerCycle, fastMovesPerCycle, dpt) ->
        putStrLn $ Printf.printf "%-*s: %d(%d) %.2f"
          maxNameLength name turnsPerCycle fastMovesPerCycle dpt
    )
    $ Exit.die

spam :: Move -> Move -> (Int, Int, Float)
spam fast charged =
  let chargedEnergy = - Move.pvpEnergyDelta charged
      fastEnergy = Move.pvpEnergyDelta fast
      fastMovesPerCycle = (chargedEnergy + fastEnergy - 1) `div` fastEnergy
      turnsPerCycle = fastMovesPerCycle * (Move.pvpDurationTurns fast + 1)
      damagePerCycle =
        (fromIntegral fastMovesPerCycle) * (Move.pvpPower fast)
        + Move.pvpPower charged
      dpt = damagePerCycle / fromIntegral turnsPerCycle
  in (turnsPerCycle, fastMovesPerCycle, dpt)
