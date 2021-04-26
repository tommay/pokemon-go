module Powerups (
  levelsAndCosts,
) where

import qualified Cost
import           Cost (Cost)
import qualified Discounts
import           Discounts (Discounts)
import qualified GameMaster
import           GameMaster (GameMaster)

import           Data.List as List

-- Returns [(level, Cost)] starting at the given level, which is zero cost
-- to power up to since it's the current level.
--
levelsAndCosts :: GameMaster -> Discounts -> Float -> [(Float, Cost)]
levelsAndCosts gameMaster discounts level =
  let filteredLevelAndCost = filter ((>= level) . fst) $
        GameMaster.allLevelAndCost gameMaster
      (levels, costs) = unzip filteredLevelAndCost
      discountedCosts = map (Discounts.apply discounts) costs
      runningTotal = List.scanl' (<>) mempty discountedCosts
      poweredUpLevels = case levels of
        [] -> []
        (head:_) -> head : map (+ 0.5) levels
  in zip poweredUpLevels runningTotal
