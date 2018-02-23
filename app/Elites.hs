module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified Matchup
import           Matchup (Matchup)
import qualified Util

import           Control.Monad (join)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Exit as Exit
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  elitesOnly :: Bool,
  filterNames :: Bool,
  filename   :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optElitesOnly <*> optHypothetical <*> optFilename
      optElitesOnly = O.switch
        (  O.long "elites"
        <> O.short 'e'
        <> O.help "Filter non-elites from all output")
      optHypothetical = O.switch
        (  O.long "hypothetical"
        <> O.short 'h'
        <> O.help "Filter out hypothetical pokemon (with [.+] in their name)")
      optFilename = O.strOption
        (  O.long "file"
        <> O.short 'f'
        <> O.metavar "FILE"
        <> O.value "matchups.out"
        <> O.showDefault
        <> O.help "File to read matchup data from")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc ("Use matchups.out to find elite pokemon and their" ++
             "victims"))
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      matchups <- do
        matchups <- join $ Matchup.load $ filename options
        return $ case filterNames options of
          False -> matchups
          True -> filter
            (Maybe.isNothing .
             Regex.matchRegex (Regex.mkRegex "\\[.+\\]") .
             Matchup.attacker)
            matchups

      -- byDefender maps a defender to a list of its matchups.

      let byDefender :: HashMap String [Matchup]
          byDefender = Util.groupBy Matchup.defender matchups

          -- Find the matchups with the most damage and decent dps.

          eliteMatchups :: [Matchup]
          eliteMatchups = concat
            $ map (keepTopDamageMatchups . keepHighDpsMatchups)
            $ HashMap.elems byDefender  -- [[Matchup]]

          -- HashMap (attacker, _, _) [Matchup]
          eliteAttackers = Util.groupBy attackerInfo eliteMatchups

          -- HashMap (attacker, _, _) [String]
          eliteVictims = HashMap.map (map Matchup.defender) eliteAttackers

          -- An attacker is considered elite only if it has elite victims.
          filterElites blah =
            let getAttacker (attacker, _, _) = attacker
                attackers = map getAttacker $ HashMap.keys blah
                filtered = HashMap.map (filter (`elem` attackers)) blah
                filtered' = HashMap.filter (not . null) filtered
            in if blah == filtered'
                 then blah
                 else filterElites filtered'

          filteredEliteAttackers = if elitesOnly options
            then filterElites eliteVictims
            else eliteVictims

          getAttacker ((attacker, _, _), _) = attacker

          sorted = Util.sortWith getAttacker $
            HashMap.toList filteredEliteAttackers

      mapM_ (putStrLn . showElite) $ sorted
    )
    $ Exit.die

showElite :: ((String, String, String), [String]) -> String
showElite ((attacker, quick, charge), victims) =
  let sortedVictims = List.intercalate ", " $ List.sort victims
  in Printf.printf "%s %s / %s => %s" attacker quick charge sortedVictims

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
