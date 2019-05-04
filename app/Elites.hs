-- Generic has something to do with making Attacker an instance of Hashable.
{-# LANGUAGE DeriveGeneric #-} -- For deriving Hashable instance.

module Main where

-- Read matchups.out and output the elite attackers + moveset and their
-- victims.
--
-- To be an alite against a particular defender an attacker needs to
-- in the top 10 percentile in dps and must have tdo >= 90% of the
-- best tdo.
--
-- XXX Perhaps this should just be about dps since that's what mostly
-- matters for raids.

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified Matchup
import           Matchup (Matchup)
import qualified Util

import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)

import           Control.Monad (join)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Exit as Exit
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

import qualified Debug as D

data Options = Options {
  elitesOnly :: Bool,
  topNByDps :: Int,
  filterRedundant :: Bool,
  filterNames :: Bool,
  filename   :: String,
  excludes :: [String]
}

-- Attacker is name, quickName, chargeName.

data Attacker = Attacker String String String
  deriving (Eq, Generic)

instance Hashable Attacker

getOptions :: IO Options
getOptions =
  let opts = Options <$> optElitesOnly <*> optN <*> optFilterRedundant
        <*> optHypothetical <*> optFilename <*> optExcludes
      optElitesOnly = O.switch
        (  O.long "elites"
        <> O.short 'e'
        <> O.help "Filter non-elites from all output")
      optN = O.option O.auto
        (  O.long "top"
        <> O.short 'n'
        <> O.metavar "N"
        <> O.value 10
        <> O.help "Select the top N pokemon by dps for each defender")
      optFilterRedundant = O.switch
        (  O.long "noredundant"
        <> O.short 'r'
        <> O.help "Filter redundant attackers")
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
      optExcludes = (O.many . O.strOption)
        (  O.long "exclude"
        <> O.short 'x'
        <> O.metavar "POKEMON"
        <> O.help "Pokemon to exclude from elite consideration")
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

      allMatchups <- do
        allMatchups <- join $ Matchup.load $ filename options
        allMatchups <- do
          return $
            filter (not . (`elem` excludes options) . Matchup.attacker)
              allMatchups
        return $ case filterNames options of
          False -> allMatchups
          True -> filter
            (Maybe.isNothing .
             Regex.matchRegex (Regex.mkRegex "\\[.+\\]") .
             Matchup.attacker)
            allMatchups

      -- matchupsByDefender maps a defender to a list of its Matchups.

      let matchupsByDefender :: HashMap String [Matchup]
          matchupsByDefender = Util.groupBy Matchup.defender allMatchups

          -- Keep Matchups against a defender with best dps and decent
          -- damage.

          eliteMatchups :: [Matchup]
          eliteMatchups = concat
            $ map (keepHighDpsMatchups $ topNByDps options)
            $ HashMap.elems matchupsByDefender  -- [[Matchup]]

          -- HashMap Attacker [Matchup]
          eliteMatchupsByAttacker =
            Util.groupBy getAttackerFromMatchup eliteMatchups

          -- HashMap Attacker [String]
          victimsByAttacker =
            HashMap.map (map Matchup.defender) eliteMatchupsByAttacker

          filterRedundant = Main.filterRedundant options

      victimsByAttacker <- do
        return $
          HashMap.filter
            (not . (filterRedundant &&) . isRedundant victimsByAttacker)
            victimsByAttacker

      let sorted = List.sortOn (\ (Attacker attacker _ _, _) -> attacker)
            $ HashMap.toList victimsByAttacker

      mapM_ (putStrLn . showElite) $ sorted
    )
    $ Exit.die

showElite :: (Attacker, [String]) -> String
showElite (Attacker attacker quick charge, victims) =
  let sortedVictims = List.intercalate ", " $ List.sort victims
  in Printf.printf "%s %s / %s => %s" attacker quick charge sortedVictims

getAttackerFromMatchup :: Matchup -> Attacker
getAttackerFromMatchup matchup = Attacker
  (Matchup.attacker matchup)
  (Matchup.quick matchup)
  (Matchup.charge matchup)

-- Keep the top ten percentile of Matchups by dps.  This eliminates
-- attackers with low dps even if they do a lot of damage by having
-- high bulk, e.g., snorlax.
--
keepHighDpsMatchups :: Int -> [Matchup] -> [Matchup]
keepHighDpsMatchups n matchups =
  let sortedByDps = reverse $ List.sortOn Matchup.dps matchups
  in take n sortedByDps

-- Keep Matchups with damage >= 90% of the maximum damage.  This may
-- keep only one Matchup if no other attacker even comes close to the
-- maximum.
--
keepTopDamageMatchups :: [Matchup] -> [Matchup]
keepTopDamageMatchups matchups =
  let damageCutOff =
        (List.maximum $ map Matchup.minDamage matchups) * 9 `div` 10
  in filter ((>= damageCutOff) . Matchup.minDamage) matchups

-- An attacker is outclassed if there another attacker whose victim
-- list is a strict superset of the given attacker's victim list.  It
-- dodesn't matter what a given attacker is outclassed by, just that
-- it is strictly outclassed.
--
isRedundant :: HashMap Attacker [String] -> [String] -> Bool
isRedundant victimsByAttacker victims =
  List.any (`isStrictSupersetOf` victims)
    $ HashMap.elems victimsByAttacker

isStrictSupersetOf :: Eq a => [a] -> [a] -> Bool
superset `isStrictSupersetOf` subset =
  length superset > length subset && List.all (`elem` superset) subset
