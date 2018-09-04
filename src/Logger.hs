-- Logger is the Writer monad but it hides the fact that it maintains
-- a List of things.  I.e., instead of declaring "Writer [w] a" it's
-- just "Logger w a", and similarly it's just "Logger.log w" instead
-- of "Writer.tell [w]".

module Logger (
  Logger,
  Logger.log,
  runLogger,
  execLogger,
  logger,
) where

import qualified Control.Monad.Trans.Writer as Writer
import           Control.Monad.Trans.Writer (Writer)

type Logger w = Writer [w]

log :: w -> Logger w ()
log w = Writer.tell [w]

runLogger :: Logger w a -> (a, [w])
runLogger = Writer.runWriter

execLogger :: Logger w a -> [w]
execLogger = Writer.execWriter

logger :: (a, [w]) -> Logger w a
logger = Writer.writer
