{-# LANGUAGE DeriveGeneric #-} -- For deriving Hashable instance.

module Weather (
  Weather (..),
) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data Weather =
    Clear
  | Fog
  | Overcast
  | PartlyCloudy
  | Rainy
  | Snow
  | Windy
  deriving (Eq, Show, Generic)

instance Hashable Weather
