module Main where

import qualified Epic
import qualified Matchup
import           Matchup (Matchup)
import qualified Util

import           Control.Monad (join)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import qualified System.IO as IO
import qualified Text.Printf as Printf

main =
  Epic.catch (
    do
      matchups <- join $ Matchup.load "matchups.out"

      -- byDefender maps a defender to a list of its matchups.

      let byDefender :: HashMap String [Matchup]
          byDefender = Util.groupBy Matchup.defender matchups

      -- Find the matchups with the most damage and decent dps.

          eliteMatchups :: [Matchup]
          eliteMatchups = concat
            $ map (keepTopDamageMatchups . keepHighDpsMatchups)
            $ HashMap.elems byDefender  -- [[Matchup]]

          eliteAttackers = HashMap.toList $
            Util.groupBy attackerInfo eliteMatchups

          sorted = Util.sortWith (\((attacker, _, _), _) -> attacker)
            eliteAttackers

      mapM_ (putStrLn . showElite) $ sorted
    )
    $ IO.hPutStrLn IO.stderr

showElite :: ((String, String, String), [Matchup]) -> String
showElite ((attacker, quick, charge), matchups) =
  let defenders = List.intercalate ", " $
        List.sort $ map Matchup.defender matchups
  in Printf.printf "%s %s / %s => %s" attacker quick charge defenders

attackerInfo :: Matchup -> (String, String, String)
attackerInfo matchup =
  (Matchup.attacker matchup, Matchup.quick matchup, Matchup.charge matchup)

-- Keep the top ten percentile of Matchups by dps.  This eliminates
-- attackers with low dps even if they do a lot of damage by having
-- high bulk, e.g., snorlax.
--
keepHighDpsMatchups :: [Matchup] -> [Matchup]
keepHighDpsMatchups matchups =
  let sortedByDps = reverse $ Util.sortWith Matchup.dps matchups
      dpsCutoff = Matchup.dps $ sortedByDps !! (length sortedByDps `div` 10)
  in takeWhile ((>= dpsCutoff) . Matchup.dps) sortedByDps

-- Keep Matchups with damage >= 90% of the maximum damage.  This may
-- keep only one Matchup if no other attacker even comes close to the
-- maximum.
--
keepTopDamageMatchups :: [Matchup] -> [Matchup]
keepTopDamageMatchups matchups =
  let damageCutOff =
        (List.maximum $ map Matchup.minDamage matchups) * 9 `div` 10
  in filter ((>= damageCutOff) . Matchup.minDamage) matchups
