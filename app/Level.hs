module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import           PokemonBase (PokemonBase)

import qualified Data.Range.Range as Range
import           Data.Range.Range (Range (SpanRange))
import qualified System.IO as I
import qualified Text.Printf as Printf

data Options = Options {
  ivRange :: Range Int,
  cp      :: Maybe Int,
  species :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optIvRange <*> optCp <*> optSpecies
      optIvRange = optTwo <|> pure (SpanRange 0 45)
      optTwo = O.flag' (SpanRange 30 45)
        (  O.long "two"
        <> O.short '2'
        <> O.help "Show levels for IVs that are at least \"strong\"")
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
  in O.customExecParser prefs options

main = Epic.catch (
  do
    options <- getOptions

    gameMaster <- do
      ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
      ioGameMaster

    pokemonBase <- GameMaster.getPokemonBase gameMaster $
      species options

    let ivRange = Main.ivRange options

    case cp options of
      Nothing -> printAllLevels gameMaster pokemonBase ivRange
      Just cp -> printLevelsForCp gameMaster pokemonBase ivRange cp
  )
  $ \ex -> I.hPutStrLn I.stderr $ ex

levels = [1..40]  -- Catches can npw exceed maxEncounterPlayerLevel (30)
                  -- depending on the weather.

printAllLevels :: GameMaster -> PokemonBase -> Range Int -> IO ()
printAllLevels gameMaster pokemonBase ivRange = do
  mapM_ (\level -> do
          let (min, max) = cpMinMax gameMaster pokemonBase ivRange level
          putStrLn $ Printf.printf "%2d: %4d %4d" level min max)
    levels

printLevelsForCp :: GameMaster -> PokemonBase -> Range Int -> Int -> IO ()
printLevelsForCp gameMaster pokemonBase ivRange cp = do
  let levels' = filter (\level ->
          let (min, max) = cpMinMax gameMaster pokemonBase ivRange level
          in min <= cp && cp <= max)
        levels
      min = minimum levels'
      max = maximum levels'
   in putStrLn $ show min ++ " - " ++ show max

cpMinMax :: GameMaster -> PokemonBase -> Range Int -> Int -> (Int, Int)
cpMinMax gameMaster pokemonBase ivRange level =
  let ivs = possibleIvs ivRange
      cpMultiplier = GameMaster.getCpMultiplier gameMaster (fromIntegral level)
      cp (attack, defense, stamina) =
        Calc.cp pokemonBase cpMultiplier attack defense stamina
      cps = map cp ivs
      min = minimum cps
      max = maximum cps
  in (min, max)

possibleIvs :: Range Int -> [(Int, Int, Int)]
possibleIvs ivRange =
  [(attack, defense, stamina) |
    let ivs = [0 .. 15],
    attack <- ivs, defense <- ivs, stamina <- ivs,
    Range.inRange ivRange (attack + defense + stamina)]
