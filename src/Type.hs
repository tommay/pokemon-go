module Type (
  Type (Type),
  name,
  effectiveness,
  stab
) where

import StringMap (StringMap)

data Type = Type {
  name          :: String,
  effectiveness :: StringMap Float,
  stab          :: Float
} deriving (Eq, Show)
