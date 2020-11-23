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
import qualified PokeUtil

import           Control.Monad (join)
import qualified Data.Maybe as Maybe
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

    gameMaster <- join $ GameMaster.load

    pokemonBase <- GameMaster.getPokemonBase gameMaster $
      species options

    let ivFloor' = Maybe.fromMaybe 0 $ Main.ivFloor options

    case cp options of
      Nothing -> printAllLevels gameMaster pokemonBase ivFloor'
      Just cp -> printLevelsForCp gameMaster pokemonBase ivFloor' cp
  )
  $ Exit.die

printAllLevels :: GameMaster -> PokemonBase -> Int -> IO ()
printAllLevels gameMaster pokemonBase ivFloor =
  mapM_ (\level -> do
          let (min, max) = cpMinMax gameMaster pokemonBase ivFloor level
          putStrLn $ Printf.printf "%-4s: %4d %4d"
            (PokeUtil.levelToString level) min max)
    $ GameMaster.allLevels gameMaster

printLevelsForCp :: GameMaster -> PokemonBase -> Int -> Int -> IO ()
printLevelsForCp gameMaster pokemonBase ivFloor cp =
  let levels = filter (\level ->
          let (min, max) = cpMinMax gameMaster pokemonBase ivFloor level
          in min <= cp && cp <= max)
        $ GameMaster.allLevels gameMaster
      min = minimum levels
      max = maximum levels
   in putStrLn $
        PokeUtil.levelToString min ++ " - " ++ PokeUtil.levelToString max

cpMinMax :: GameMaster -> PokemonBase -> Int -> Float -> (Int, Int)
cpMinMax gameMaster pokemonBase ivFloor level =
  let ivs = possibleIvs ivFloor
      ivs' = map (\ (attack, defense, stamina) ->
        IVs.new level attack defense stamina) ivs
      cps = map (Calc.cp gameMaster pokemonBase) ivs'
      min = minimum cps
      max = maximum cps
  in (min, max)

possibleIvs :: Int -> [(Int, Int, Int)]
possibleIvs ivFloor =
  [(attack, defense, stamina) |
    let ivs = [ivFloor .. 15],
    attack <- ivs, defense <- ivs, stamina <- ivs]
