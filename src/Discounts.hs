module Discounts (
  Discounts (Discounts),
  noDiscounts,
  powerupStardust,
  powerupCandy,
  new
) where

import qualified Text.Show.Functions

import qualified GameMaster
import           GameMaster (GameMaster)

data Discounts = Discounts {
  powerupStardust :: Int -> Int,
  powerupCandy :: Int -> Int
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
  powerupCandy = id,
  powerupStardust = id
}

shadowDiscounts gameMaster = noDiscounts {
  -- Not sure if this is rounded or what.  The candy cost for a level
  -- 13 pokemon is 2, for a shadow pokemon it is 3.  The multiplier is
  -- 1.2.
  powerupCandy = (+1) .
    (discount $ GameMaster.shadowCandyMultiplier gameMaster),
  -- Stardust definitely has the +1.
  powerupStardust = (+1) . (discount $ GameMaster.shadowStardustMultiplier gameMaster)
}

-- A normal level 25 Charmander would be:
--   powerup: 4000 dust, 3 candy
-- A purified level 25 Charmander (with a multiplier of 0.9) is showing:
--   powerup: 3600 dust, 3 candy
--
purifiedDiscounts gameMaster = Discounts {
  powerupStardust =
    discount $ GameMaster.purifiedStardustMultiplier gameMaster,
  -- XXX not sure this is rounded or what:
  powerupCandy = roundedDiscount $ GameMaster.purifiedCandyMultiplier gameMaster
}

luckyDiscounts gameMaster = noDiscounts {
  powerupStardust = discount $
    1 - GameMaster.luckyPowerUpStardustDiscountPercent gameMaster
}

luckyPurifiedDiscounts gameMaster = (purifiedDiscounts gameMaster) {
  -- XXX Not sure this is correct.
  powerupStardust = discount $
    GameMaster.luckyPowerUpStardustDiscountPercent gameMaster *
      GameMaster.purifiedStardustMultiplier gameMaster
}
