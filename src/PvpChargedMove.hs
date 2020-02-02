module PvpChargedMove (
  PvpChargedMove,
  new,
  moveType,
  power,
  setLegacy,
  isLegacy,
) where

import qualified Type
import           Type (Type)
import qualified Util

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Text.Regex as Regex

data PvpChargedMove = PvpChargedMove {
  uniqueId :: String,
  moveType :: Type,
  power    :: Float,
  energyDelta :: Int,
  isLegacy :: Bool
}

instance Show PvpChargedMove where
  show = uniqueId

new = PvpChargedMove

{-
name :: PvpChargedMove -> String
name this =
  let noFast = Regex.subRegex (Regex.mkRegex "_FAST$") (uniqueId this) ""
      alphaOnly = Regex.subRegex (Regex.mkRegex "[^a-zA-Z]") noFast " "
      withType = if PvpFastMove.isHiddenPower this
        then alphaOnly ++ ", " ++ (Type.name $ PvpFastMove.moveType this)
        else alphaOnly
      lower = Util.toLower alphaOnly
  in lower
-}

setLegacy :: PvpChargedMove -> PvpChargedMove
setLegacy this =
  this { isLegacy = True }
