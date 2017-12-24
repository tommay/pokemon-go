module Logger (
  Logger,
  Logger.log,
  runLogger,
  execLogger,
  logger,
) where

import qualified Control.Monad.Writer as Writer
import           Control.Monad.Writer (Writer)

type Logger w = Writer [w]

log :: w -> Logger w ()
log w = Writer.tell [w]

runLogger :: Logger w a -> (a, [w])
runLogger =  Writer.runWriter

execLogger :: Logger w a -> [w]
execLogger =  Writer.execWriter

logger :: (a, [w]) -> Logger w a
logger = Writer.writer
