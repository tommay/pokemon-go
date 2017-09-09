module Type (
  Type (Type),
) where

import Data.Text (Text)
import TextMap (TextMap)

data Type = Type {
  effectiveness :: TextMap Float,
  name          :: Text,
  stab          :: Float
} deriving (Show)
