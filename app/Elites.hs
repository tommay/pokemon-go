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

      let byDefender :: HashMap String [Matchup]
          byDefender = Util.groupBy Matchup.defender matchups

      let eliteMatchups :: [Matchup]
          eliteMatchups = concat $ HashMap.elems $ HashMap.map
            (keepTopMatchups . keepHighDpsMatchups)
            byDefender

      let eliteAttackers = Util.groupBy attackerInfo eliteMatchups

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

keepHighDpsMatchups :: [Matchup] -> [Matchup]
keepHighDpsMatchups matchups =
  let sortedByDps = reverse $ Util.sortWith Matchup.dps matchups
      dpsCutoff = Matchup.dps $ sortedByDps !! (length sortedByDps `div` 10)
  in filter ((>= dpsCutoff) . Matchup.dps) matchups

keepTopMatchups :: [Matchup] -> [Matchup]
keepTopMatchups matchups =
  let damageCutOff =
        (List.maximum $ map Matchup.minDamage matchups) * 9 `div` 10
  in filter ((>= damageCutOff) . Matchup.minDamage) matchups
