module Discounts (
  Discounts (Discounts),
  noDiscounts,
  powerupStardust,
  powerupCandy,
  new
) where

import qualified Text.Show.Functions

data Discounts = Discounts {
  powerupCandy :: Int -> Int,
  powerupStardust :: Int -> Int,
  evolveCandy :: Int -> Int,
  newMoveCandy :: Int -> Int,
  newMoveStardust :: Int -> Int
} deriving (Show)

new :: Bool -> Bool -> Bool -> Discounts
new shadow purified lucky =
  if shadow then
    if purified then
      error "Can't be both shadow and purified"
    else
      if lucky then
        error "Lucky shadow isn't handled"
      else
        shadowDiscounts
  else
    if purified then
      if lucky then
        luckyPurifiedDiscounts
      else
        purifiedDiscounts
    else
      if lucky then
        luckyDiscounts
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
  powerupStardust = id,
  evolveCandy = id,
  newMoveCandy = id,
  newMoveStardust = id
}

-- Shadow pokemon are 3x more expensive for everything.
--
shadowDiscounts = noDiscounts {
  powerupCandy = (*3),
  powerupStardust = (*3),
  newMoveCandy = (*3),
  newMoveStardust = (*3)
}

-- A normal level 25 Charmander would be:
--   powerup: 4000 dust, 3 candy
--   evolve: 25 candy
--   new attack: 10000 dust, 25 candy
-- A purified level 25 Charmander is showing:
--   powerup: 3600 dust, 3 candy
--   evolve: 22 candy
--   new attack: 8000 dust, 20 candy
--
purifiedDiscounts = Discounts {
  powerupCandy = roundedDiscount 0.9,    -- XXX not sure this is correct.
  powerupStardust = discount 0.9,
  evolveCandy = discount 0.9,
  newMoveCandy = discount 0.8,
  newMoveStardust = discount 0.8
}

luckyDiscounts = noDiscounts {
  powerupStardust = discount 0.5
}

luckyPurifiedDiscounts = purifiedDiscounts {
  -- XXX Not sure this is correct.
  powerupStardust = discount 0.45
}
