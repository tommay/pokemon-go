module Discounts (
  Discounts (Discounts),
  noDiscounts,
  apply,
  new
) where

import qualified Text.Show.Functions

import qualified Cost
import           Cost (Cost)
import qualified GameMaster
import           GameMaster (GameMaster)

data Discounts = Discounts {
  stardustDiscount :: Int -> Int,
  candyDiscount :: Int -> Int
} deriving (Show)

new :: GameMaster -> Bool -> Bool -> Bool -> Discounts
new gameMaster shadow purified lucky =
  if shadow then
    if purified then
      error "Can't be both shadow and purified"
    else
      if lucky then
        error "Shadows can't be lucky because they can't be traded"
      else
        shadowDiscounts gameMaster
  else
    if purified then
      if lucky then
        luckyPurifiedDiscounts gameMaster
      else
        purifiedDiscounts gameMaster
    else
      if lucky then
        luckyDiscounts gameMaster
      else
        noDiscounts

discount :: Float -> Int -> Int
discount factor =
  floor . (factor *) . fromIntegral

roundedDiscount :: Float -> Int -> Int
roundedDiscount factor =
  round . (factor *) . fromIntegral

noDiscounts = Discounts {
  stardustDiscount = id,
  candyDiscount = id
}

shadowDiscounts gameMaster = noDiscounts {
  -- XXX The stardust costs are weird and inconsistent.  For some values in
  -- game_master the shadow cost is *1.2, for ssome it is *1.2 + 1, sometimes
  -- it is based in the value for the previous or next level, and it isn't
  -- consistent when comparing a level 1 zubat and a level 8 zubat.
  -- Not sure if this is rounded or what.  The candy cost for a level
  -- 13 pokemon is 2, for a shadow pokemon it is 3.  The multiplier is
  -- 1.2.
  candyDiscount = (+1) .
    (discount $ GameMaster.shadowCandyMultiplier gameMaster),
  -- Stardust definitely has the +1.
  stardustDiscount = (+1) . (discount $ GameMaster.shadowStardustMultiplier gameMaster)
}

-- A normal level 25 Charmander would be:
--   powerup: 4000 dust, 3 candy
-- A purified level 25 Charmander (with a multiplier of 0.9) is showing:
--   powerup: 3600 dust, 3 candy
--
purifiedDiscounts gameMaster = Discounts {
  stardustDiscount =
    discount $ GameMaster.purifiedStardustMultiplier gameMaster,
  -- XXX not sure this is rounded or what:
  candyDiscount = roundedDiscount $ GameMaster.purifiedCandyMultiplier gameMaster
}

luckyDiscounts gameMaster = noDiscounts {
  stardustDiscount = discount $
    1 - GameMaster.luckyPowerUpStardustDiscountPercent gameMaster
}

luckyPurifiedDiscounts gameMaster = (purifiedDiscounts gameMaster) {
  -- XXX Not sure this is correct.
  stardustDiscount = discount $
    GameMaster.luckyPowerUpStardustDiscountPercent gameMaster *
      GameMaster.purifiedStardustMultiplier gameMaster
}

apply :: Discounts -> Cost -> Cost
apply this cost =
  Cost.new (stardustDiscount this $ Cost.dust cost)
    (candyDiscount this $ Cost.candy cost)
    (Cost.xlCandy cost)
