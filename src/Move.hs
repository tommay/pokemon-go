module Move (
  Move (Move),
) where

import Type (Type)

data Move = Move {
  moveType :: Type,
  power    :: Float,
  duration :: Float,
  energy   :: Float
} deriving (Show)
