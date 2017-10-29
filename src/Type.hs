module Type (
  Type,
  new,
  typeId,
  stabFor,
  effectivenessAgainst,
  name,
) where

import StringMap (StringMap)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Regex as Regex

data Type = Type {
  typeId        :: String,
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
  product $ map
    (\ptype -> HashMap.lookupDefault 1 (Type.typeId ptype)
      (effectiveness this))
    defenderTypes

name :: Type -> String
name this =
  -- Get rid of the POKEMON_TYPE_.
  let regex = Regex.mkRegex ".*_"
  in map Char.toLower $ Regex.subRegex regex (Type.typeId this) ""
