module Epic (
  Epic.catch,
  Epic.fail,
  E.SomeException,
  E.MonadThrow,
) where

import qualified Control.Monad.Catch as E
import           Data.Typeable (Typeable)

data EpicException = EpicException String
  deriving (Show, Typeable)
instance E.Exception EpicException

fail :: E.MonadThrow m => String -> m a
fail = E.throwM . EpicException

catch expr handler =
  E.catch expr (\ (EpicException ex) -> handler ex)
