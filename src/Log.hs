module Log (
  Log (Log),
  state,
  what,
  makeLogs,
) where

import qualified Logger
import           Logger (Logger)

data Log a = Log {
  state :: a,
  what :: String
} deriving (Show)

-- This takes the [String] that the Logger has accumulated and turns
-- it into [Log a] by turning each String into a Log a with the
-- original String and the given a.
--
-- This is useful when it takes several steps, each with a logged
-- "what" String, to produce a final a state.  Each String ends up
-- associated with the final a which may not be ideal, instead perhaps
-- there should be a [String] associated with the final a, but at some
-- point it seemed like something I would find useful when reading log
-- output.
--
-- XXX It's not clear whether this belongs here, i.e., whether Log should
-- know about the Logger type.
--
makeLogs :: Logger String a -> Logger (Log a) a
makeLogs logger =
  let makeLog a string = Log {
        Log.state = a,
        Log.what = string
        }
  in decorateLog makeLog logger

decorateLog :: (a -> w -> u) -> Logger w a -> Logger u a
decorateLog f logger =
  let (a, ws) = Logger.runLogger logger
      us = map (f a) ws
  in Logger.logger (a, us)
