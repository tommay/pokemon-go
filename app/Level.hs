module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Epic
import qualified IVs
import qualified GameMaster
import           GameMaster (GameMaster)
import           PokemonBase (PokemonBase)

import           Control.Monad (join)
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  cp      :: Maybe Int,
  ivFloor :: Maybe Int,
  species :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optCp <*> optIvFloor <*> optSpecies
      optCp = O.optional $ O.option O.auto
        (  O.long "cp"
        <> O.short 'c'
        <> O.metavar "CP"
        <> O.help "Print possible levels for the given CP")
      optIvFloor = O.optional $ O.option O.auto
        (  O.long "ivFloor"
        <> O.short 'm'
        <> O.metavar "N"
        <> O.help "Set minimum IV, e.g., 10 for raid boss or hatch")
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Map levels to CP range for a species.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main = Epic.catch (
  do
    options <- getOptions

    gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

    pokemonBase <- GameMaster.getPokemonBase gameMaster $
      species options

    let ivPred = case Main.ivFloor options of
          Just ivFloor -> (>= ivFloor)
          Nothing -> const True

    case cp options of
      Nothing -> printAllLevels gameMaster pokemonBase ivPred
      Just cp -> printLevelsForCp gameMaster pokemonBase ivPred cp
  )
  $ Exit.die

levels = [1..40]  -- Catches can now exceed maxEncounterPlayerLevel (30)
                  -- depending on the weather.

printAllLevels :: GameMaster -> PokemonBase -> (Int -> Bool) -> IO ()
printAllLevels gameMaster pokemonBase ivPred =
  mapM_ (\level -> do
          let (min, max) = cpMinMax gameMaster pokemonBase ivPred level
          putStrLn $ Printf.printf "%2d: %4d %4d" level min max)
    levels

printLevelsForCp :: GameMaster -> PokemonBase -> (Int -> Bool) -> Int -> IO ()
printLevelsForCp gameMaster pokemonBase ivPred cp =
  let levels' = filter (\level ->
          let (min, max) = cpMinMax gameMaster pokemonBase ivPred level
          in min <= cp && cp <= max)
        levels
      min = minimum levels'
      max = maximum levels'
   in putStrLn $ show min ++ " - " ++ show max

cpMinMax :: GameMaster -> PokemonBase -> (Int -> Bool) -> Int -> (Int, Int)
cpMinMax gameMaster pokemonBase ivPred level =
  let ivs = possibleIvs ivPred
      ivs' = map (\ (attack, defense, stamina) ->
        IVs.new (fromIntegral level) attack defense stamina) ivs
      cps = map (Calc.cp gameMaster pokemonBase) ivs'
      min = minimum cps
      max = maximum cps
  in (min, max)

possibleIvs :: (Int -> Bool) -> [(Int, Int, Int)]
possibleIvs ivPred =
  [(attack, defense, stamina) |
    let ivs = [0 .. 15],
    attack <- ivs, defense <- ivs, stamina <- ivs,
    ivPred (attack + defense + stamina)]
