module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import Data.List as List

data Options = Options {
  level :: Float
}

getOptions :: IO Options
getOptions =
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
        <> O.progDesc "Show stardust costs to get to various levels.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main = do
  options <- getOptions
  let makeLevelsAndRunningCost cost =
        let levels = [1, 1.5 ..]
            cost2x = concat $ map (replicate 2) cost
            levelAndCost = zip levels cost2x
            filteredLevelAndCost = filter (\ (lvl, _) -> lvl >= level options)
              levelAndCost
            (levels', cost') = unzip filteredLevelAndCost
            runningTotal = List.scanl' (+) 0 cost'
        in (levels', runningTotal)
      (levels', stardustCosts) = makeLevelsAndRunningCost stardustCost
      (_, candyCosts) = makeLevelsAndRunningCost candyCost
  mapM_ (\ (level, dust, candy) -> putStrLn $
    show level ++ ": " ++ show dust ++ " " ++ show candy)
    $ zip3 levels' stardustCosts candyCosts

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

-- From GAME_MASTER.yaml candyCost:

candyCost :: [Int]
candyCost = [
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  3,
  3,
  3,
  3,
  3,
  4,
  4,
  4,
  4,
  4,
  6,
  6,
  8,
  8,
  10,
  10,
  12,
  12,
  15,
  15
  ]
