-- Generic has something to do with making Weather an instance of Hashable.
{-# LANGUAGE DeriveGeneric #-} -- For deriving Hashable instance.  Also
  -- for deriving Store.Store (see GameMaster.hs).

module Weather (
  Weather (..),
  optWeather,
) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import qualified Options.Applicative as O
import           Options.Applicative ((<|>))
import           Data.Semigroup ((<>))

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

optWeather :: O.Parser Weather
optWeather =
      O.flag' Clear (O.long "clear" <> O.help "weather is sunny/clear")
  <|> O.flag' Clear (O.long "sunny" <> O.help "weather is sunny/clear")
  <|> O.flag' Fog (O.long "fog")
  <|> O.flag' Overcast (O.long "cloudy")
  <|> O.flag' PartlyCloudy (O.long "partly")
  <|> O.flag' Rainy (O.long "rainy")
  <|> O.flag' Snow (O.long "snow")
  <|> O.flag' Windy (O.long "windy")
