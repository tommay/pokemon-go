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
  summary     :: Range Int,
  best        :: [String],
  bestRange   :: Range Int
} deriving (Show)

new :: (Epic.MonadCatch m) => String -> m Appraisal
new string = do
  let words = split " *, *" string
  (summary', words) <- getSummary words
  (best', words) <- getBest words
  (bestRange', words) <- getBestRange words
  case words of
    [] -> return $ Appraisal summary' best' bestRange'
    _ -> Epic.fail $ "junk at the end of appraisal"

possibleIvs :: Appraisal -> [(Int, Int, Int)]
possibleIvs this =
  [(attack, defense, stamina) |
    let ivs = [0 .. 15],
    attack <- ivs, defense <- ivs, stamina <- ivs,
    ok this attack defense stamina]

getSummary :: (Epic.MonadCatch m) => [String] -> m (Range Int, [String])
getSummary words =
  let
    getRange phrase
      | phrase `elem` ["not likely", "may not", "room"] = range 0 22
      | phrase `elem` ["above", "decent", "decent"]     = range 23 29
      | phrase `elem` ["attention", "strong", "strong"] = range 30 36
      | phrase `elem` ["wonder", "amazes", "best"]      = range 37 45
      | otherwise = Epic.fail $ "bad summary in appraisal"
  in case words of
    summary : rest -> do
      range <- getRange summary
      return $ (range, rest)
    _ -> Epic.fail $ "missing summary in appraisal"

range :: (Monad m) => Int -> Int -> m (Range Int)
range low high =
  return $ SpanRange low high

getBest :: (Epic.MonadCatch m) => [String] -> m ([String], [String])
getBest words =
  let (best, rest) = span (`elem` ["attack", "defense", "hp"]) words
      best' = List.sort best
  in case best' of
    [] -> Epic.fail $ "no best IV given in appraisal"
    _ ->
      if best' /= List.nub best'
        then Epic.fail $ "duplicate IV name in appraisal"
        else return $ (best', rest)

getBestRange :: (Epic.MonadCatch m) => [String] -> m (Range Int, [String])
getBestRange words =
  let
    getRange phrase
      | phrase `elem` ["not out", "point", "basic"]            = range 0 7
      | phrase `elem` ["trending", "indicate", "definitely"]   = range 8 12
      | phrase `elem` ["impressed", "excellent", "impressive"] = range 13 14
      | phrase `elem` ["incredible", "blown", "no doubt"]      = range 15 15
      | otherwise = Epic.fail $ "bad best IV description in appraisal"
  in case words of
       phrase : rest -> do
         range <- getRange phrase
         return $ (range, rest)
       _ -> Epic.fail $ "missing best IV description in appraisal"

ok :: Appraisal -> Int -> Int -> Int -> Bool
ok this attack defense stamina =
  (attack + defense + stamina) `inRange` summary this &&
  checkBestRange this attack defense stamina &&
  checkBest this attack defense stamina

checkBestRange :: Appraisal -> Int -> Int -> Int -> Bool
checkBestRange this attack defense stamina =
  let bestValue = case head $ best this of
        "attack" -> attack
        "defense" -> defense
        "hp" -> stamina
  in bestValue `inRange` bestRange this

checkBest :: Appraisal -> Int -> Int -> Int -> Bool
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

-- inRange can be used sensibly as an infix operator.
--
inRange :: (Ord a) => a -> Range a -> Bool
inRange n range = Range.inRange range n
