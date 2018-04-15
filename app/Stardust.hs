module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)

import           Control.Monad (join, forM_)
import           Data.List as List
import qualified System.Exit as Exit

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

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      let makeRunningCostAndLevels costsAndLevel =
            let filteredCostAndLevel =
                  filter (\ (_, lvl) -> lvl >= level options) costsAndLevel
                (costs, levels) = unzip filteredCostAndLevel
                runningTotal = List.scanl' (+) 0 costs
            in (runningTotal, levels)
          (stardustCosts, levels) =
            makeRunningCostAndLevels $ GameMaster.dustAndLevel gameMaster
          (candyCosts, _) =
            makeRunningCostAndLevels $ GameMaster.candyAndLevel gameMaster
      forM_ (zip3 levels stardustCosts candyCosts) $ \ (level, dust, candy) ->
        putStrLn $ show level ++ ": " ++ show dust ++ " " ++ show candy
    )
    $ Exit.die
