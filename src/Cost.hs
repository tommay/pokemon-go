module Cost (
  Cost,
  dust,
  candy,
  new,
) where

data Cost = Cost {
  dust :: Int,
  candy :: Int
} deriving (Show)

instance Semigroup Cost where
  Cost dust candy <> Cost dust' candy' =
    Cost (dust + dust') (candy + candy')

instance Monoid Cost where
  mempty = Cost 0 0

new = Cost
