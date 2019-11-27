module Powerups (
  levelsAndCosts,
) where

import qualified Discounts
import           Discounts (Discounts)
import qualified GameMaster
import           GameMaster (GameMaster)

import           Data.List as List

-- Returns [(level, stardustCost, candyCost)]
--
levelsAndCosts :: GameMaster -> Discounts -> Float -> [(Float, Int, Int)]
levelsAndCosts gameMaster discounts level =
  let makeRunningCostAndLevels discount costsAndLevel =
        let filteredCostAndLevel =
              filter (\ (_, lvl) -> lvl >= level) costsAndLevel
            (costs, levels) = unzip filteredCostAndLevel
            runningTotal = List.scanl' (+) 0 $ map discount costs
        in (runningTotal, levels)
      (stardustCosts, levels) =
        makeRunningCostAndLevels (Discounts.powerupStardust discounts)
          $ GameMaster.dustAndLevel gameMaster
      (candyCosts, _) =
        makeRunningCostAndLevels (Discounts.powerupCandy discounts)
          $ GameMaster.candyAndLevel gameMaster
  in zip3 levels stardustCosts candyCosts
