module Main where

import qualified Epic
import qualified Matchup
import           Matchup (Matchup)

import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable (Hashable)
import qualified Data.List as List
import qualified System.IO as IO
import qualified Text.Printf as Printf

main =
  Epic.catch (
    do
      matchups <- do
        ioMatchups <- Matchup.load "matchups.out"
        ioMatchups

      let byDefender :: HashMap String [Matchup]
          byDefender = groupBy Matchup.defender matchups

      let eliteMatchups :: [Matchup]
          eliteMatchups = concat $ HashMap.elems $ HashMap.map
            (keepTopMatchups . keepHighDpsMatchups)
            byDefender

      let eliteAttackers = groupBy attackerInfo eliteMatchups

      mapM_ (putStrLn . showElite) $ HashMap.toList eliteAttackers
    )
    $ \ex -> IO.hPutStrLn IO.stderr ex

showElite :: ((String, String, String), [Matchup]) -> String
showElite ((attacker, quick, charge), matchups) =
  let defenders = List.intercalate ", " $ map Matchup.defender matchups
  in Printf.printf "%s %s / %s => %s" attacker quick charge defenders

attackerInfo :: Matchup -> (String, String, String)
attackerInfo matchup =
  (Matchup.attacker matchup, Matchup.quick matchup, Matchup.charge matchup)

-- There are a number of ways to implement something like this on
-- https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
-- Seems like there should be in a library somewhere.  It sure took a lot
-- of extraneous bookkeeping to get this little function building.

groupBy :: (Hashable b, Eq b) => (a -> b) -> [a] -> HashMap b [a]
groupBy fn lst =
  foldr (\ v m -> HashMap.insertWith (++) (fn v) [v] m)
    HashMap.empty
    lst

compareWith :: Ord b => (a -> b) -> a -> a -> Ordering
compareWith f first second =
  f first `compare` f second

keepHighDpsMatchups :: [Matchup] -> [Matchup]
keepHighDpsMatchups matchups =
  let sortedByDps = reverse $ List.sortBy (compareWith Matchup.dps) matchups
      dpsCutoff = Matchup.dps $ sortedByDps !! (length sortedByDps `div` 10)
  in filter ((>= dpsCutoff) . Matchup.dps) matchups

keepTopMatchups :: [Matchup] -> [Matchup]
keepTopMatchups matchups =
  let damageCutOff =
        (List.maximum $ map Matchup.minDamage matchups) * 9 `div` 10
  in filter ((>= damageCutOff) . Matchup.minDamage) matchups
