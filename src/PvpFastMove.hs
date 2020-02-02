module PvpFastMove (
  PvpFastMove,
  new,
  moveType,
  power,
  durationTurns,
  energyDelta,
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

data PvpFastMove = PvpFastMove {
  uniqueId :: String,
  moveType :: Type,
  power    :: Float,
  durationTurns :: Int,
  energyDelta   :: Int,
  isLegacy :: Bool
}

instance Show PvpFastMove where
  show = uniqueId

new = PvpFastMove

{-
name :: PvpFastMove -> String
name this =
  let noFast = Regex.subRegex (Regex.mkRegex "_FAST$") (uniqueId this) ""
      alphaOnly = Regex.subRegex (Regex.mkRegex "[^a-zA-Z]") noFast " "
      withType = if PvpFastMove.isHiddenPower this
        then alphaOnly ++ ", " ++ (Type.name $ PvpFastMove.moveType this)
        else alphaOnly
      lower = Util.toLower alphaOnly
  in lower
-}

isHiddenPower :: PvpFastMove -> Bool
isHiddenPower this =
  PvpFastMove.uniqueId this == "HIDDEN_POWER_FAST"

-- This is for "hidden power", which has a type specific to an
-- individual Pokemon.
--
setType :: PvpFastMove -> Type -> PvpFastMove
setType this moveType =
  this { moveType = moveType }

setLegacy :: PvpFastMove -> PvpFastMove
setLegacy this =
  this { isLegacy = True }
