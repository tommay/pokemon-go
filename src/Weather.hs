{-# LANGUAGE DeriveGeneric #-} -- For deriving Hashable instance.

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
  <|> O.flag' Fog (O.long "fog")
  <|> O.flag' Overcast (O.long "overcast")
  <|> O.flag' PartlyCloudy (O.long "partlycloudy")
  <|> O.flag' Rainy (O.long "rainy")
  <|> O.flag' Snow (O.long "snow")
  <|> O.flag' Windy (O.long "windy")
