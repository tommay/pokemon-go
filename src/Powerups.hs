module Powerups (
  levelsAndCosts,
) where

import qualified GameMaster
import           GameMaster (GameMaster)

import           Data.List as List

-- Returns [(level, stardustCost, candyCost)]
--
levelsAndCosts :: GameMaster -> Float -> [(Float, Int, Int)]
levelsAndCosts gameMaster level =
  let makeRunningCostAndLevels costsAndLevel =
        let filteredCostAndLevel =
              filter (\ (_, lvl) -> lvl >= level) costsAndLevel
            (costs, levels) = unzip filteredCostAndLevel
            runningTotal = List.scanl' (+) 0 costs
        in (runningTotal, levels)
      (stardustCosts, levels) =
        makeRunningCostAndLevels $ GameMaster.dustAndLevel gameMaster
      (candyCosts, _) =
        makeRunningCostAndLevels $ GameMaster.candyAndLevel gameMaster
  in zip3 levels stardustCosts candyCosts
