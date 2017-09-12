module Move (
  Move (Move),
  power,
  duration,
  energy,
  isCharge,
  isQuick,
  stabFor,
  effectivenessAgainst
) where

import qualified Type
import           Type (Type)

import qualified Data.HashMap.Strict as HashMap

data Move = Move {
  moveType :: Type,
  power    :: Float,
  duration :: Float,
  energy   :: Float
} deriving (Eq, Show)

isCharge :: Move -> Bool
isCharge this =
  Move.energy this < 0

isQuick :: Move -> Bool
isQuick this =
  not $ Move.isCharge this

stabFor :: Move -> [Type] -> Float
stabFor this attackerTypes =
  case Move.moveType this `elem` attackerTypes of
    True -> Type.stab $ Move.moveType this
    False -> 1.0

-- XXX lookupDefault
effectivenessAgainst :: Move -> [Type] -> Float
effectivenessAgainst this defenderTypes =
  let effectiveness = Type.effectiveness $ Move.moveType this
  in foldr (\ptype accum ->
             accum * HashMap.lookupDefault 1 (Type.name ptype) effectiveness)
       1
       defenderTypes
