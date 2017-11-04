module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)

import qualified System.IO as I
import qualified Text.Printf as Printf

data Options = Options {
  species :: String
}

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optSpecies
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Map levels to CP range for a species."
        <> O.header "Map levels to CP range for a species.")
      prefs = O.prefs O.showHelpOnEmpty
  O.customExecParser prefs options

main = Epic.catch (
  do
    options <- getOptions

    gameMaster <- do
      ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
      ioGameMaster

    pokemonBase <- GameMaster.getPokemonBase gameMaster $
      species options

    let levels = [1..30]   -- maxEncounterPlayerLevel: 30, max catch level.
    mapM_ (\level -> do
            let cpMultiplier = GameMaster.getCpMultiplier gameMaster level
                min = Calc.cp pokemonBase cpMultiplier 0 0 0
                max = Calc.cp pokemonBase cpMultiplier 15 15 15
            putStrLn $ Printf.printf "%2d: %4d %4d" ((floor level) :: Int) min max)
      levels
  )
  $ \ex -> I.hPutStrLn I.stderr $ ex
