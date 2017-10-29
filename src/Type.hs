module Type (
  Type,
  new,
  name,
  stabFor,
  effectivenessAgainst,
) where

import StringMap (StringMap)

import qualified Data.HashMap.Strict as HashMap

data Type = Type {
  name          :: String,
  effectiveness :: StringMap Float,
  stab          :: Float
} deriving (Eq, Show)

new = Type

stabFor :: Type -> [Type] -> Float
stabFor this attackerTypes =
  case this `elem` attackerTypes of
    True -> stab this
    False -> 1.0

effectivenessAgainst :: Type -> [Type] -> Float
effectivenessAgainst this defenderTypes =
  product $ map (\ptype -> HashMap.lookupDefault 1 (Type.name ptype) (effectiveness this)) defenderTypes
