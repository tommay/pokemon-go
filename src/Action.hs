module Action (
  Action (Action),
  what,
) where

data Action = Action {
  what :: String
} deriving (Show)
