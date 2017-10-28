module Pokemon (
  Pokemon,
  new,
  pname,
  species,
  types,
  attack,
  defense,
  stamina,
  quick,
  charge,
  hp,
  possibleMoves,
) where

import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import           Type (Type)

data Pokemon = Pokemon {
  pname       :: String,
  species     :: String,
  types       :: [Type],
  attack      :: Float,
  defense     :: Float,
  stamina     :: Float,
  quick       :: Move,
  charge      :: Move,
  base        :: PokemonBase
} deriving (Show)

new = Pokemon

hp :: Pokemon -> Int
hp this = floor $ stamina this

possibleMoves :: Pokemon -> [Move]
possibleMoves this =
  let pokemonBase = Pokemon.base this
  in concat $ sequence [PokemonBase.quickMoves, PokemonBase.chargeMoves] pokemonBase
