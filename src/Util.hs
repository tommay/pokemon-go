module Util (
  compareWith,
  sortWith,
  Util.groupBy,
) where

import Data.List as List
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)

compareWith :: Ord b => (a -> b) -> a -> a -> Ordering
compareWith f first second =
  f first `compare` f second

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith fn =
  List.sortBy (Util.compareWith fn)

-- There are a number of ways to implement something like this on
-- https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
-- Seems like there should be in a library somewhere.  It sure took a lot
-- of extraneous bookkeeping to get this little function building.

groupBy :: (Hashable b, Eq b) => (a -> b) -> [a] -> HashMap b [a]
groupBy fn lst =
  foldr (\ v m -> HashMap.insertWith (++) (fn v) [v] m)
    HashMap.empty
    lst
