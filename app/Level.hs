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
  ivPred  :: Int -> Bool,
  cp      :: Maybe Int,
  species :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optIvPred <*> optCp <*> optSpecies
      optIvPred = optTwo <|> pure (const True)
      optTwo = O.flag' (>= 30)
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

    let ivPred = Main.ivPred options

    case cp options of
      Nothing -> printAllLevels gameMaster pokemonBase ivPred
      Just cp -> printLevelsForCp gameMaster pokemonBase ivPred cp
  )
  $ \ex -> I.hPutStrLn I.stderr $ ex

levels = [1..40]  -- Catches can npw exceed maxEncounterPlayerLevel (30)
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
      cpMultiplier = GameMaster.getCpMultiplier gameMaster (fromIntegral level)
      cp (attack, defense, stamina) =
        Calc.cp pokemonBase cpMultiplier attack defense stamina
      cps = map cp ivs
      min = minimum cps
      max = maximum cps
  in (min, max)

possibleIvs :: (Int -> Bool) -> [(Int, Int, Int)]
possibleIvs ivPred =
  [(attack, defense, stamina) |
    let ivs = [0 .. 15],
    attack <- ivs, defense <- ivs, stamina <- ivs,
    ivPred (attack + defense + stamina)]
