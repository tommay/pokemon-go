{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Debug

import           Control.Monad (forM_)
import qualified Data.List.Ordered as Ordered
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto
import qualified System.Exit as Exit

data Options = Options {
  filenames :: [FilePath],
  evalByAttack :: Bool,
  cheapestByCandy :: Bool,
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
  let opts = Options <$> optFilenames <*> optEvalByAttack
        <*> optCheapestByCandy
        <*> optShowSorted <*> optShowKeep <*> optShowDiscard
      optFilenames = O.some $ O.strArgument
        (O.metavar "FILENAME"
        <> O.help "file with output from \"bulk\"")
      optEvalByAttack = O.switch
        (  O.long "attack"
        <> O.short 'a'
        <> O.help "Evaluate by attack instead of stat product")
      optCheapestByCandy = O.switch
        (  O.long "candy"
        <> O.short 'c'
        <> O.help "Determine cheapest by candy instead of stardust")
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
        filenames = Main.filenames options
    -- contents :: [[String]]
    contents <- mapM readLines filenames
    case mapM (mapM parseStuff) contents of
      Left error -> putStrLn error
      -- stuffs :: [[Stuff]]
      Right stuffs -> do
        -- examineds :: [([Stuff], [Stuff], [Stuff])]
        let evalField = if evalByAttack options then attack else statProduct
            compareGoodness a b =
              evalField a `Ord.compare` evalField b
            compareCost a b =
              let (primary, secondary) = if cheapestByCandy options
                    then (candy, stardust)
                    else (stardust, candy)
              in case primary a `Ord.compare` primary b of
                EQ -> case secondary a `Ord.compare` secondary b of
                  EQ -> evalField b `Ord.compare` evalField a
                  x -> x
                x -> x
            examineds = map (examineStuff compareCost compareGoodness) stuffs
            showLines = mapM_ (putStrLn . text)
        forM_ (zip filenames examineds) $
          \(filename, (sorted, keep, discard)) -> do
            let showIf pred string =
                  if pred then putStrLn string else return ()
            showIf (length filenames > 1) $ "--- " ++ filename ++ ":"
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
        if (showDefault || showDiscard options) && length filenames > 1
          then do
            let discards = map (\(_, _, d) -> d) examineds
                isSame a b = description a == description b
                discardFromAll = foldr1 (List.intersectBy isSame) discards
            if length discardFromAll > 0
              then do
                putStrLn "discard from all:"
                showLines discardFromAll
              else pure ()
          else pure ()

-- -> ([sorted], [keep], [discard])
--
examineStuff :: (Stuff -> Stuff -> Ordering) -> (Stuff -> Stuff -> Ordering) ->
  [Stuff] -> ([Stuff], [Stuff], [Stuff])
examineStuff compareCost compareGoodness stuffs =
  let sorted = List.sortBy compareCost stuffs
      -- nubBy will keep only the elements with increasing goodness,
      -- I.e., as we go through the list towards more expensive pokemon
      -- we only keep them if they are better.
      keep = Ordered.nubBy (\ a b -> compareGoodness a b == LT) sorted
      discard = discardedBy (Ord.comparing description) sorted keep
  in (sorted, keep, discard)

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
        -- We've parsed what we need, just ignore the rest.
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

compareStardust :: Ord a => (Stuff -> a) -> Stuff -> Stuff -> Ordering
compareStardust evalField a b =
  case stardust a `Ord.compare` stardust b of
    GT -> GT
    EQ -> case candy a `Ord.compare` candy b of
      GT -> GT
      EQ -> evalField b `Ord.compare` evalField a
      LT -> LT
    LT -> LT

-- Given an old list and a new list with some elements discarded, return
-- the elements that were discarded.
--
discardedBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
discardedBy compareTo old new =
  let loop old [] = old
      loop [] _ = error "discardedBy: old list emptied before new list"
      loop (a:as) old@(b:bs) =
        case a `compareTo` b of
          EQ -> loop as bs
          _ -> a : loop as old
  in loop old new
