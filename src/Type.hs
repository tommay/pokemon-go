module Type (
  Type (Type),
) where

import StringMap (StringMap)

data Type = Type {
  effectiveness :: StringMap Float,
  name          :: String,
  stab          :: Float
} deriving (Show)
