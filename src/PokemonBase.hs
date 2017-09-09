module PokemonBase (
  PokemonBase (PokemonBase),
) where

import qualified Data.Text as Text
import           Data.Text (Text)

import qualified Type
import           Type (Type)
import qualified Move
import           Move (Move)

data PokemonBase = PokemonBase {
  types        :: [Type],
  attack       :: Int,
  defense      :: Int,
  stamina      :: Int,
  evolutions   :: [Text],
  quickMoves   :: [Move],
  chargeMoves  :: [Move],
  parent       :: Maybe Text
} deriving (Show)

