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
isCharge this =
  Move.energy this < 0

isQuick :: Move -> Bool
isQuick this =
  not $ Move.isCharge this

