module Move (
  Move (Move),
  moveType,
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
  Type.stabFor (Move.moveType this) attackerTypes

effectivenessAgainst :: Move -> [Type] -> Float
effectivenessAgainst this defenderTypes =
  Type.effectivenessAgainst (Move.moveType this) defenderTypes
