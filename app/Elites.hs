{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Epic

import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable (Hashable)
import qualified Data.List as List
import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:), (.:?))
import qualified System.IO as IO
import qualified Text.Printf as Printf

data Result = Result {
  defender :: String,
  attacker :: String,
  quick :: String,
  charge :: String,
  dps :: Float,
  minDamage :: Int,
  maxDamage :: Int
} deriving (Show)

instance Yaml.FromJSON Result where
  parseJSON = Yaml.withObject "Result" $ \y ->
    Result <$>
    y .: "defender" <*>
    y .: "attacker" <*>
    y .: "quick" <*>
    y .: "charge" <*>
    y .: "dps" <*>
    y .: "minDamage" <*>
    y .: "maxDamage"

load :: Epic.MonadCatch m => FilePath -> IO (m [Result])
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Right results -> return $ pure results
    Left yamlParseException ->
      Epic.fail $ Yaml.prettyPrintParseException yamlParseException

main =
  Epic.catch (
    do
      results <- do
        ioResults <- load "attackerresults.out"
        ioResults

      let byDefender = groupBy defender results
          sorted = HashMap.map
            (reverse .
             List.sortBy (compareWith minDamage) .
             keepHighDpsResults)
            byDefender

      let elites = map head (HashMap.elems sorted)

      mapM_ (putStrLn . showAttacker) elites
    )
    $ \ex -> IO.hPutStrLn IO.stderr ex

showAttacker :: Result -> String
showAttacker result =
  Printf.printf "%.1f %d %s %s / %s (%s)"
    (dps result) (minDamage result) (attacker result)
    (quick result) (charge result) (defender result)

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

keepHighDpsResults :: [Result] -> [Result]
keepHighDpsResults results =
  let sortedByDps = reverse $ List.sortBy (compareWith dps) results
      dpsCutoff = dps $ sortedByDps !! (length sortedByDps `div` 10)
  in filter ((>= dpsCutoff) . dps) results
