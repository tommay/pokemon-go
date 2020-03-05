module Move (
  Move,
  new,
  moveType,
  power,
  duration,
  durationMs,
  damageWindow,
  energy,
  pvpPower,
  pvpEnergyDelta,
  pvpDurationTurns,
  isCharge,
  isQuick,
  stabFor,
  effectivenessAgainst,
  name,
  bars,
  isHiddenPower,
  setType,
  setLegacy,
  isLegacy,
) where

import qualified Type
import           Type (Type)
import qualified Util

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Text.Regex as Regex

data Move = Move {
  movementId :: String,
  moveType :: Type,
  power    :: Float,
  duration :: Float,
  damageWindow :: Int,
  energy   :: Int,
  pvpPower :: Float,
  pvpEnergyDelta :: Int,
  pvpDurationTurns :: Int,
  isLegacy :: Bool
}

instance Eq Move where
  left == right = (movementId left) == (movementId right)

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
      withType = if Move.isHiddenPower this
        then alphaOnly ++ ", " ++ (Type.name $ Move.moveType this)
        else alphaOnly
      lower = Util.toLower alphaOnly
      withLegacy = lower ++ if isLegacy this then "*" else ""
  in withLegacy

bars :: Move -> Int
bars this =
  if Move.isCharge this
    then case Move.energy this of
      -33 -> 3
      -50 -> 2
      -100 -> 1
      _ ->  error $ "Unknown energy for " ++ Move.name this
    else  error $
      "`" ++ Move.name this ++ "' is not a charge move and has no bars"

isHiddenPower :: Move -> Bool
isHiddenPower this =
  Move.movementId this == "HIDDEN_POWER_FAST"

-- This is for "hidden power", which has a type specific to an
-- individual Pokemon.
--
setType :: Move -> Type -> Move
setType this moveType =
  this { moveType = moveType }

setLegacy :: Move -> Move
setLegacy this =
  this { isLegacy = True }
