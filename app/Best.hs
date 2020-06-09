{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Debug

import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto
import qualified System.Exit as Exit

data Options = Options {
  filenames :: [FilePath]
}

data Stuff = Stuff {
  text        :: String,
  description :: String,
  stardust    :: Int,
  candy       :: Int,
  statProduct :: Double,
  attack      :: Double
} deriving (Show)

testString :: String
testString = "1315 10 1 12:   97000/93  : 26   21.10   104.90"

main =
  do
    let eitherStuff = parseStuff testString
    putStrLn $ show eitherStuff

-- The string looks like
-- 1315 10 1 12:   97000/93  : 26   21.10   104.90
--
parseStuff :: String -> Either String Stuff
parseStuff string =
  let attoParseStuff = do
        description <- Text.unpack <$> Atto.takeWhile (/= ':')
        Atto.char ':'
        Atto.skipSpace
        stardust <- Atto.decimal
        Atto.char '/'
        candy <- Atto.decimal
        Atto.skipSpace
        Atto.char ':'
        Atto.skipSpace
        Atto.double       -- level, unused
        Atto.skipSpace
        statProduct <- Atto.double
        Atto.skipSpace
        attack <- Atto.double
        Atto.endOfInput
        return $ Stuff {
          text = string,
          description = description,
          stardust = stardust,
          candy = candy,
          statProduct = statProduct,
          attack = attack
          }
  in case Atto.parseOnly attoParseStuff (Text.pack string) of
    Left error -> Left $ "Error parsing '" ++ string ++ "':\n" ++ error
    Right stuff -> Right stuff
