module PokemonBase (
  PokemonBase,
  new,
  species,
  types,
  attack,
  defense,
  stamina,
  evolutions,
  quickMoves,
  chargeMoves,
  hasEvolutions,
  baseCaptureRate,
  addMove,
) where

import           Type (Type)
import qualified Move
import           Move (Move)

import           Data.Text (Text)

data PokemonBase = PokemonBase {
  species      :: String,
  types        :: [Type],
  attack       :: Int,
  defense      :: Int,
  stamina      :: Int,
  evolutions   :: [(String, Int)],
  quickMoves   :: [Move],
  chargeMoves  :: [Move],
  parent       :: Maybe String,
  baseCaptureRate :: Float
}

instance Show PokemonBase where
  show = species

new = PokemonBase

hasEvolutions :: PokemonBase -> Bool
hasEvolutions this = (not . null) $ PokemonBase.evolutions this

-- add*Move are for adding legacy moves.

addMove :: Move -> PokemonBase -> PokemonBase
addMove move this =
  if Move.isQuick move
    then this { quickMoves = move : quickMoves this }
    else this { chargeMoves = move : chargeMoves this }
