module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import           PokemonBase (PokemonBase)

import qualified System.IO as I
import qualified Text.Printf as Printf

data Options = Options {
  cp      :: Maybe Int,
  species :: String
}

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optCp <*> optSpecies
      optCp = O.optional $ O.option O.auto
        (  O.long "cp"
        <> O.short 'c'
        <> O.metavar "CP"
        <> O.help "Print possible levels for the given CP")
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

    case cp options of
      Nothing -> printAllLevels gameMaster pokemonBase
      Just cp -> printLevelsForCp gameMaster pokemonBase cp
  )
  $ \ex -> I.hPutStrLn I.stderr $ ex

levels = [1..30]   -- maxEncounterPlayerLevel: 30, max catch level.

printAllLevels :: GameMaster -> PokemonBase -> IO ()
printAllLevels gameMaster pokemonBase = do
  mapM_ (\level -> do
          let (min, max) = cpMinMax gameMaster pokemonBase level
          putStrLn $ Printf.printf "%2d: %4d %4d" level min max)
    levels

printLevelsForCp :: GameMaster -> PokemonBase -> Int -> IO ()
printLevelsForCp gameMaster pokemonBase cp = do
  let levels' = filter (\level ->
          let (min, max) = cpMinMax gameMaster pokemonBase level
          in min <= cp && cp <= max)
        levels
      min = minimum levels'
      max = maximum levels'
   in putStrLn $ show min ++ " - " ++ show max

cpMinMax :: GameMaster -> PokemonBase -> Int -> (Int, Int)
cpMinMax gameMaster pokemonBase level =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster (fromIntegral level)
      min = Calc.cp pokemonBase cpMultiplier 0 0 0
      max = Calc.cp pokemonBase cpMultiplier 15 15 15
  in (min, max)
