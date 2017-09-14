module PokemonBase (
  PokemonBase (PokemonBase),
  species,
  types,
  attack,
  defense,
  stamina,
  quickMoves,
  chargeMoves,
) where

import qualified Data.Text as Text
import           Data.Text (Text)

import qualified Type
import           Type (Type)
import qualified Move
import           Move (Move)

data PokemonBase = PokemonBase {
  species      :: String,
  types        :: [Type],
  attack       :: Integer,
  defense      :: Integer,
  stamina      :: Integer,
  evolutions   :: [Text],
  quickMoves   :: [Move],
  chargeMoves  :: [Move],
  parent       :: Maybe Text
} deriving (Show)

