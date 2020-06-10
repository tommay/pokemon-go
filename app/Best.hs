{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Debug

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto
import qualified System.Exit as Exit

data Options = Options {
  filename :: FilePath,
  showSorted :: Bool,
  showKeep :: Bool,
  showDiscard :: Bool
}

data Stuff = Stuff {
  text        :: String,
  description :: String,
  stardust    :: Int,
  candy       :: Int,
  statProduct :: Double,
  attack      :: Double
} deriving (Show)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilepath
        <*> optShowSorted <*> optShowKeep <*> optShowDiscard
      optFilepath = O.strArgument
        (O.metavar "FILENAME"
        <> O.help "file with output from \"bulk\"")
      optShowSorted = O.switch
        (  O.long "sorted"
        <> O.short 's'
        <> O.help "Show sorted lined")
      optShowKeep = O.switch
        (  O.long "keep"
        <> O.short 'k'
        <> O.help "Show pokemon to keep")
      optShowDiscard = O.switch
        (  O.long "discard"
        <> O.short 'd'
        <> O.help "Show pokemon to discard")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Determine pokemon worth keeping for pvp based on \"bulk\" output")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  do
    options <- getOptions
    let showDefault =
          not $ (showSorted options || showKeep options || showDiscard options)
    lines <- readLines $ filename options
    case mapM parseStuff lines of
      Left error -> putStrLn error
      Right stuffs -> do
        let sorted = List.sortBy compareStardust stuffs
            keep = runningBestBy (Ord.comparing statProduct) sorted
            discard = discardedBy (Ord.comparing description) sorted keep
            showLines = mapM_ (putStrLn . text)
        if showSorted options || showDefault
          then do
            showIf (showKeep options || showDiscard options || showDefault) "sorted:"
            showLines sorted
          else pure ()
        if showKeep options || showDefault
          then do
            showIf (showSorted options || showDiscard options || showDefault) "keep:"
            showLines keep
          else pure ()
        if showDiscard options || showDefault
          then do
            showIf (showSorted options || showKeep options || showDefault) "discard:"
            showLines discard
          else pure ()

showIf :: Bool -> String -> IO ()
showIf pred string =
  if pred then putStrLn string else return ()

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

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

compareStardust :: Stuff -> Stuff -> Ordering
compareStardust a b =
  case stardust a `Ord.compare` stardust b of
    GT -> GT
    EQ -> candy a `Ord.compare` candy b
    LT -> LT

-- Keep the items that are the best seen so far.
--
runningBestBy :: (a -> a -> Ordering) -> [a] -> [a]
runningBestBy _ [] = []
runningBestBy _ [a] = [a]
runningBestBy compareTo (a:b:as) = case a `compareTo` b of
  LT -> a : runningBestBy compareTo (b:as)  -- keep a as the former best and move on
  _ -> runningBestBy compareTo (a:as)       -- a is better, keep it and discard b

-- Given an old list and a new list with some elements discarded, return
-- the elements that were discarded.
--
discardedBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
discardedBy _ old [] = old
discardedBy _ [] _ = error "old list emptied before new list"
discardedBy compareTo (a:as) (b:bs) =
   case a `compareTo` b of
     EQ -> discardedBy compareTo as bs
     _ -> a : discardedBy compareTo as (b:bs)
