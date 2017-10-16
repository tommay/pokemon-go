module Appraisal (
  new,
  possibleIvs
) where

import qualified Epic

import qualified Data.List as List
import qualified Data.Range.Range as Range
import           Data.Range.Range (Range (SpanRange))
import qualified Text.Regex as Regex

data Appraisal = Appraisal {
  summary     :: Range Integer,
  best        :: [String],
  bestRange   :: Range Integer
} deriving (Show)

new :: (Epic.MonadCatch m) => String -> m Appraisal
new string = do
  let words = split ", *" string
  summary' <- getSummary words
  best' <- getBest words
  bestRange' <- getBestRange words
  return $ Appraisal summary' best' bestRange'

possibleIvs :: Appraisal -> [(Integer, Integer, Integer)]
possibleIvs this =
  [(attack, defense, stamina) |
    let ivs = [0 .. 15],
    attack <- ivs, defense <- ivs, stamina <- ivs,
    ok this attack defense stamina]

getSummary words
  | includes words ["not likely", "may not"] = return $ SpanRange 0 22
  | includes words ["above", "decent"] = return $ SpanRange 23 29
  | includes words ["attention", "strong"] = return $ SpanRange 30 36
  | includes words ["wonder", "amazes"] = return $ SpanRange 37 45
  | otherwise = Epic.fail $ "Bad summary in appraisal"

getBest :: (Epic.MonadCatch m) => [String] -> m [String]
getBest words = do
  let best = List.sort $
        filter (\w -> w `elem` ["attack", "defense", "hp"]) words
      len = length best
  case len `inRange` (SpanRange 1 3) && len == (length $ List.nub best) of
    True -> return $ best
    False -> Epic.fail "Bad best IV in appraisal"

getBestRange words
  | includes words ["not out", "point"] = return $ SpanRange 0 7
  | includes words ["trending", "indicate"] = return $ SpanRange 8 12
  | includes words ["impressed", "excellent"] = return $ SpanRange 13 14
  | includes words ["incredible", "blown"] = return $ SpanRange 15 15
  | otherwise = Epic.fail $ "Bad best IV description in appraisal"

ok :: Appraisal -> Integer -> Integer -> Integer -> Bool
ok this attack defense stamina =
  (attack + defense + stamina) `inRange` summary this &&
  checkBestRange this attack defense stamina &&
  checkBest this attack defense stamina

checkBestRange :: Appraisal -> Integer -> Integer -> Integer -> Bool
checkBestRange this attack defense stamina =
  let bestValue = case head $ best this of
        "attack" -> attack
        "defense" -> defense
        "hp" -> stamina
  in bestValue `inRange` bestRange this

checkBest :: Appraisal -> Integer -> Integer -> Integer -> Bool
checkBest this attack defense stamina =
  case best this of
    ["attack"] ->
      attack > defense && attack > stamina
    ["attack", "defense"] ->
      attack == defense && attack > stamina
    ["attack", "hp"] ->
      attack == stamina && attack > defense
    ["attack", "defense", "hp"] ->
      attack == defense && attack == stamina
    ["defense"] ->
      defense > attack && defense > stamina
    ["defense", "hp"] ->
      defense == stamina && defense > attack
    ["hp"] ->
      stamina > attack && stamina > defense

split :: String -> String -> [String]
split delim = Regex.splitRegex $ Regex.mkRegex delim

includes :: [String] -> [String] -> Bool
includes words list =
  any (\w -> w `elem` list) words

-- inRange can be used sensibly as an infix operator.
--
inRange :: (Ord a) => a -> Range a -> Bool
inRange n range = Range.inRange range n
