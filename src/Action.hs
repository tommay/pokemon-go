module Action (
  Action (Action),
  state,
  what,
) where

data Action a = Action {
  state :: a,
  what :: String
} deriving (Show)
