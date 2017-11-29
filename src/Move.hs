module Move (
  Move,
  new,
  moveType,
  power,
  duration,
  durationMs,
  damageWindow,
  energy,
  isCharge,
  isQuick,
  stabFor,
  effectivenessAgainst,
  name,
  isHiddenPower,
  setType
) where

import qualified Type
import           Type (Type)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Text.Regex as Regex

data Move = Move {
  movementId :: String,
  moveType :: Type,
  power    :: Float,
  duration :: Float,
  damageWindow :: Int,
  energy   :: Int
} deriving (Eq)

instance Show Move where
  show = movementId

new = Move

isCharge :: Move -> Bool
isCharge this =
  not $ Move.isQuick this

isQuick :: Move -> Bool
isQuick this =
  List.isSuffixOf "_FAST" $ movementId this

stabFor :: Move -> [Type] -> Float
stabFor this attackerTypes =
  Type.stabFor (Move.moveType this) attackerTypes

effectivenessAgainst :: Move -> [Type] -> Float
effectivenessAgainst this defenderTypes =
  Type.effectivenessAgainst (Move.moveType this) defenderTypes

durationMs :: Move -> Int
durationMs this =
  round $ Move.duration this * 1000

name :: Move -> String
name this =
  let noFast = Regex.subRegex (Regex.mkRegex "_FAST$") (movementId this) ""
      alphaOnly = Regex.subRegex (Regex.mkRegex "[^a-zA-Z]") noFast " "
      lower = map Char.toLower alphaOnly
  in if Move.isHiddenPower this
       then lower ++ " (" ++ (Type.name $ Move.moveType this) ++ ")"
       else lower

isHiddenPower :: Move -> Bool
isHiddenPower this =
  Move.movementId this == "HIDDEN_POWER_FAST"

-- This is for "hidden power", which has a type specific to an
-- individual Pokemon.
--
setType :: Move -> Type -> Move
setType this moveType =
  this { moveType = moveType }
