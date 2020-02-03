{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PvpChargedMove
import           PvpChargedMove (PvpChargedMove)
import qualified PvpFastMove
import           PvpFastMove (PvpFastMove)
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
            fast <- PokemonBase.pvpFastMoves base,
            charged <- PokemonBase.pvpChargedMoves base]
          toName :: PvpFastMove -> PvpChargedMove -> String
          toName fast charged =
            Printf.printf "%s / %s"
              (PvpFastMove.name fast) (PvpChargedMove.name charged)
          toStuff :: (PvpFastMove, PvpChargedMove) -> (String, Int, Float)
          toStuff (fast, charged) =
            let name = toName fast charged
                (turnsPerCycle, dpt) = spam fast charged
            in (name, turnsPerCycle, dpt)
          stuff = map toStuff moveSets
      forM_ stuff $ \ (name, turnsPerCycle, dpt) ->
        putStrLn $ Printf.printf "%-30s: %d %.2f" name turnsPerCycle dpt
    )
    $ Exit.die

spam :: PvpFastMove -> PvpChargedMove -> (Int, Float)
spam fast charged =
  let chargedEnergy = - PvpChargedMove.energyDelta charged
      fastEnergy = PvpFastMove.energyDelta fast
      fastMovesPerCycle = (chargedEnergy + fastEnergy - 1) `div` fastEnergy
      turnsPerCycle = fastMovesPerCycle * (PvpFastMove.durationTurns fast + 1)
      damagePerCycle =
        (fromIntegral fastMovesPerCycle) * (PvpFastMove.power fast)
        + PvpChargedMove.power charged
      dpt = damagePerCycle / fromIntegral turnsPerCycle
  in (turnsPerCycle, dpt)
