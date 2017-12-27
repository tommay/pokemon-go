module Log (
  Log (Log),
  state,
  what,
) where

data Log a = Log {
  state :: a,
  what :: String
} deriving (Show)
