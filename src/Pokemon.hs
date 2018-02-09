module Pokemon (
  Pokemon,
  new,
  pname,
  species,
  ivs,
  types,
  attack,
  defense,
  stamina,
  quick,
  charge,
  hp,
  possibleMoves,
) where

import qualified IVs
import           IVs (IVs)
import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import           Type (Type)

data Pokemon = Pokemon {
  pname       :: String,
  species     :: String,
  types       :: [Type],
  ivs         :: IVs,
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
