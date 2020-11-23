module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified IVs
import qualified GameMaster
import qualified PokemonBase

import qualified Debug

import           Control.Monad (join, forM_)
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  species :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optSpecies
      optSpecies = O.argument O.str
        (O.metavar "SPECIES")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc ("Create a gnuplot script to plot absolute attack " ++
             "vs. level and atk"))
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load
      base <- GameMaster.getPokemonBase gameMaster $ Main.species options

      let attack level atkIv =
            let cpMultiplier = GameMaster.getCpMultiplier gameMaster level
            in (fromIntegral $ PokemonBase.attack base + atkIv) * cpMultiplier

          plotData = [(level, atkIv, attack level atkIv) |
            level <- [1..40], atkIv <- [0..15]]

      forM_ plotData $ \ (level, atkIv, attack) ->
        Printf.printf "%f %d %f\n" level atkIv attack
    )
    $ Exit.die
