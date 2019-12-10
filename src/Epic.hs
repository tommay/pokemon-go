module Epic (
  Epic.catch,
  Epic.fail,
  toEpic,
  E.MonadCatch,
) where

import qualified Control.Monad.Catch as E

newtype EpicException = EpicException String

instance Show EpicException where
  show (EpicException string) = string

instance E.Exception EpicException

fail :: E.MonadThrow m => String -> m a
fail = E.throwM . EpicException

catch :: E.MonadCatch m => m a -> (String -> m a) -> m a
catch expr handler =
  E.catch expr (\ (EpicException ex) -> handler ex)

toEpic :: (Show a, E.MonadCatch m) => Either a b -> m b
toEpic either =
  case either of
    Left err -> Epic.fail $ show err
    Right val -> return val
