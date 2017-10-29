module Type (
  Type (Type),
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

stabFor :: Type -> [Type] -> Float
stabFor this attackerTypes =
  case this `elem` attackerTypes of
    True -> stab this
    False -> 1.0

effectivenessAgainst :: Type -> [Type] -> Float
effectivenessAgainst this defenderTypes =
  foldr (\ptype accum ->
          accum *
            HashMap.lookupDefault 1 (Type.name ptype) (effectiveness this))
    1
    defenderTypes
