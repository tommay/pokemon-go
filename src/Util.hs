module Util (
  compareWith,
  sortWith,
) where

import Data.List as List

compareWith :: Ord b => (a -> b) -> a -> a -> Ordering
compareWith f first second =
  f first `compare` f second

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith fn =
  List.sortBy (Util.compareWith fn)
