module Debug (
  spew,
  spewWith
) where

import qualified Debug.Trace as Trace

spew :: (Show a, Show b) => a -> b -> b 
spew a b =
  Trace.traceShow (a, b) b

spewWith :: (Show a, Show c) => (b -> c) -> a -> b -> b
spewWith fn a b =
  Trace.traceShow (a, fn b) b
