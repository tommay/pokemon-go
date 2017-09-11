module Move (
  Move (Move),
  isCharge,
  isQuick
) where

import Type (Type)

data Move = Move {
  moveType :: Type,
  power    :: Float,
  duration :: Float,
  energy   :: Float
} deriving (Show)

isCharge :: Move -> Bool
isCharge move =
  Move.energy move < 0

isQuick :: Move -> Bool
isQuick move =
  not $ Move.isCharge move
