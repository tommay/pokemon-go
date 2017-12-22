module Action (
  Action (Action),
  when,
  attackerHp,
  attackerEnergy,
  defenderHp,
  defenderEnergy,
  what,
) where

-- XXX This is really crufty.  Ideally we'd just have a Battle here
-- instead of hp and energy, but the way things are set up the Battle
-- module needs to import Action which creates an import loop.  Maybe
-- there could be a separate module which imports both and runs battles
-- and creates the Writer [Action] log from Writer [String].

data Action = Action {
  when :: Int,
  attackerHp :: Int,
  attackerEnergy :: Int,
  defenderHp :: Int,
  defenderEnergy :: Int,
  what :: String
} deriving (Show)
