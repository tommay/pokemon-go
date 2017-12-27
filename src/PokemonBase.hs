module PokemonBase (
  PokemonBase,
  new,
  species,
  types,
  attack,
  defense,
  stamina,
  quickMoves,
  chargeMoves,
  moveSets,
  hasEvolutions,
  baseCaptureeRate,
) where

import           Type (Type)
import           Move (Move)

import           Data.Text (Text)

data PokemonBase = PokemonBase {
  species      :: String,
  types        :: [Type],
  attack       :: Int,
  defense      :: Int,
  stamina      :: Int,
  evolutions   :: [String],
  quickMoves   :: [Move],
  chargeMoves  :: [Move],
  parent       :: Maybe String,
  baseCaptureeRate :: Float
}

instance Show PokemonBase where
  show = species

new = PokemonBase

moveSets :: PokemonBase -> [(Move, Move)]
moveSets this =
  [(quick, charge) |
    quick <- PokemonBase.quickMoves this,
    charge <- PokemonBase.chargeMoves this]

hasEvolutions :: PokemonBase -> Bool
hasEvolutions this = (not . null) $ PokemonBase.evolutions this
