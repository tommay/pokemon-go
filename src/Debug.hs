module Debug (
  spew
) where

import qualified Debug.Trace as Trace

spew :: (Show a, Show b) => a -> b -> b 
spew a b =
  Trace.traceShow (a, b) b
