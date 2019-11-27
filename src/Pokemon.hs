module Pokemon (
  Pokemon,
  new,
  pname,
  base,
  species,
  ivs,
  types,
  level,
  attack,
  defense,
  stamina,
  quick,
  charge,
  hp,
  setName,
) where

import qualified Discounts
import           Discounts (Discounts)
import qualified IVs
import           IVs (IVs)
import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import           Type (Type)

data Pokemon = Pokemon {
  pname       :: String,
  base        :: PokemonBase,
  ivs         :: IVs,
  attack      :: Float,
  defense     :: Float,
  stamina     :: Float,
  quick       :: Move,
  charge      :: Move,
  discounts   :: Discounts
} deriving (Show)

new = Pokemon

hp :: Pokemon -> Int
hp this = floor $ stamina this

species :: Pokemon -> String
species this =
  PokemonBase.species $ base this

types :: Pokemon -> [Type]
types this =
  PokemonBase.types $ base this

level :: Pokemon -> Float
level this =
  IVs.level $ Pokemon.ivs this

setName :: String -> Pokemon -> Pokemon
setName pname this =
  this { pname = pname }
