module Epic (
  Epic.catch,
  Epic.fail,
  E.SomeException,
  E.MonadCatch,
) where

import qualified Control.Monad.Catch as E
import           Data.Typeable (Typeable)

data EpicException = EpicException String
  deriving (Show, Typeable)
instance E.Exception EpicException

fail :: E.MonadThrow m => String -> m a
fail = E.throwM . EpicException

catch :: E.MonadCatch m => m a -> (String -> m a) -> m a
catch expr handler =
  E.catch expr (\ (EpicException ex) -> handler ex)
