module Action (
  Action (Action),
) where

data Action = Action {
  clock :: Int,
  what :: String
} deriving (Show)
