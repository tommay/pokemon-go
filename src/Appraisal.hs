{-# LANGUAGE OverloadedStrings #-}

module Appraisal (
  new,
  possibleIvs
) where

import qualified Epic

import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as List
import qualified Data.Range.Range as Range
import           Data.Range.Range (Range (SpanRange))
import qualified Data.Text as Text
import           Options.Applicative ((<|>))
import qualified Text.Regex as Regex

import qualified Debug

data Appraisal = Appraisal {
  summary     :: Range Int,
  bestIvs     :: [Text.Text],
  bestRange   :: Range Int
} deriving (Show)

new :: (Epic.MonadCatch m) => String -> m Appraisal
new string =
  let parseAppraisal = do
        summary <- parseSummary
        comma
        bestIvs <- parseBestIvs
        comma
        bestRange <- parseBestRange
        Atto.endOfInput
        return $ Appraisal summary bestIvs bestRange
  in case Atto.parseOnly parseAppraisal (Text.pack string) of
    Left error -> Epic.fail $ "Bad appraisal: " ++ error
    Right appraisal -> return appraisal

comma :: Atto.Parser ()
comma = do
  Atto.char ','
  Atto.skipSpace

parseSummary :: Atto.Parser (Range Int)
parseSummary =
  Atto.choice [
    inList ["wonder", "amazes", "best"]      $ SpanRange 37 45,
    inList ["attention", "strong"]           $ SpanRange 30 36,
    inList ["above", "decent"]               $ SpanRange 23 29,
    inList ["not likely", "may not", "room"] $ SpanRange 0 22
  ]

inList :: [Text.Text] -> a -> Atto.Parser a
inList list result = do
  Atto.choice $ map Atto.string list
  return result

parseBestIvs :: Atto.Parser [Text.Text]
parseBestIvs = do
  bestIvs <- Atto.sepBy1 ("attack" <|> "defense" <|> "hp") comma
  return $ List.sort $ List.nub bestIvs

parseBestRange :: Atto.Parser (Range Int)
parseBestRange =
  Atto.choice [
    inList ["not out", "point", "basic"]            $ SpanRange 0 7,
    inList ["trending", "indicate", "definitely"]   $ SpanRange 8 12,
    inList ["impressed", "excellent", "impressive"] $ SpanRange 13 14,
    inList ["incredible", "blown", "no doubt"]      $ SpanRange 15 15
  ]

possibleIvs :: Appraisal -> [(Int, Int, Int)]
possibleIvs this =
  [(attack, defense, stamina) |
    let ivs = [0 .. 15],
    attack <- ivs, defense <- ivs, stamina <- ivs,
    ok this attack defense stamina]

ok :: Appraisal -> Int -> Int -> Int -> Bool
ok this attack defense stamina =
  (attack + defense + stamina) `inRange` summary this &&
  checkBestRange this attack defense stamina &&
  checkBestIvs this attack defense stamina

checkBestRange :: Appraisal -> Int -> Int -> Int -> Bool
checkBestRange this attack defense stamina =
  let bestValue = case head $ bestIvs this of
        "attack" -> attack
        "defense" -> defense
        "hp" -> stamina
  in bestValue `inRange` bestRange this

checkBestIvs :: Appraisal -> Int -> Int -> Int -> Bool
checkBestIvs this attack defense stamina =
  case bestIvs this of
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

-- inRange can be used sensibly as an infix operator.
--
inRange :: (Ord a) => a -> Range a -> Bool
inRange n range = Range.inRange range n
