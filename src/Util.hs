module Util (
  Util.groupBy,
  matchesAbbrevInsensitive,
  toByteString,
) where

import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import Data.Char as Char
import Data.List as List
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified System.IO as IO

-- There are a number of ways to implement something like this on
-- https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
-- Seems like there should be in a library somewhere.  It sure took a lot
-- of extraneous bookkeeping to get this little function building.

groupBy :: (Hashable b, Eq b) => (a -> b) -> [a] -> HashMap b [a]
groupBy fn lst =
  foldr (\ v m -> HashMap.insertWith (++) (fn v) [v] m)
    HashMap.empty
    lst

matchesAbbrevInsensitive :: String -> String -> Bool
matchesAbbrevInsensitive abbrev string =
  matchesAbbrev (map Char.toLower abbrev) (map Char.toLower string)

matchesAbbrev :: String -> String -> Bool
matchesAbbrev [] _ = True
matchesAbbrev _ [] = False
matchesAbbrev (a0:aa) (s0:ss) =
  a0 == s0 && matchesAbbrev' aa ss

matchesAbbrev' :: String -> String -> Bool
matchesAbbrev' [] _ = True
matchesAbbrev' _ [] = False
matchesAbbrev' a@(a0:aa) (s0:ss) =
  if a0 == s0
    then matchesAbbrev' aa ss
    else matchesAbbrev' a ss

toByteString :: Maybe FilePath -> IO ByteString
toByteString maybeFilePath =
  case maybeFilePath of
    Just filePath -> ByteString.readFile filePath
    Nothing -> ByteString.hGetContents IO.stdin
