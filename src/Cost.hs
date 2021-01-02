module Cost (
  Cost,
  new,
  dust,
  candy,
  xlCandy,
  needsXlCandy,
  comparingDust,
  comparingTotalCandy,
) where

import qualified Data.Ord as Ord

data Cost = Cost {
  dust :: Int,
  candy :: Int,
  xlCandy :: Int
} deriving (Show)

instance Semigroup Cost where
  Cost dust candy xlCandy <> Cost dust' candy' xlCandy' =
    Cost (dust + dust') (candy + candy') (xlCandy + xlCandy')

instance Monoid Cost where
  mempty = Cost 0 0 0

new = Cost

needsXlCandy :: Cost -> Bool
needsXlCandy = (> 0) . xlCandy

totalCandy :: Cost -> Int
totalCandy this = candy this + xlCandy this

comparingDust :: Cost -> Cost -> Ordering
comparingDust = Ord.comparing Cost.dust

comparingTotalCandy :: Cost -> Cost -> Ordering
comparingTotalCandy = Ord.comparing Cost.totalCandy
