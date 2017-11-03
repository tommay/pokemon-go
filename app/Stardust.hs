module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import Data.List as List

data Options = Options {
  level :: Float
}

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optLevel
      optLevel = O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.value 1.0
        <> O.showDefault
        <> O.metavar "LEVEL"
        <> O.help "Show stardust costs from LEVEL")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Show stardust costs to get to various levels."
        <> O.header "header - Show stardust costs to get to various levels.")
      prefs = O.prefs O.showHelpOnEmpty
  O.customExecParser prefs options

main = do
  options <- getOptions
  let levels = [1, 1.5 ..]
      stardustCost2x = concat $ map (replicate 2) stardustCost
      levelAndDust = filter (\ (lvl, _) -> lvl >= level options)
        $ zip levels stardustCost2x
      (levels', dust) = unzip levelAndDust
      runningTotal = List.scanl' (+) 0 dust
  mapM_ (\ (level, cost) -> putStrLn $ show level ++ ": " ++ show cost)
    $ zip levels' runningTotal


-- From GAME_MASTER.yaml stardustCost:

stardustCost :: [Int]
stardustCost = [
  200,
  200,
  400,
  400,
  600,
  600,
  800,
  800,
  1000,
  1000,
  1300,
  1300,
  1600,
  1600,
  1900,
  1900,
  2200,
  2200,
  2500,
  2500,
  3000,
  3000,
  3500,
  3500,
  4000,
  4000,
  4500,
  4500,
  5000,
  5000,
  6000,
  6000,
  7000,
  7000,
  8000,
  8000,
  9000,
  9000,
  10000,
  10000
  ]
