module PvpChargedMove (
  PvpChargedMove,
  new,
  moveType,
  power,
  energyDelta,
  setLegacy,
  isLegacy,
  name,
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

name :: PvpChargedMove -> String
name this =
  let name = uniqueId this
      alphaOnly = Regex.subRegex (Regex.mkRegex "[^a-zA-Z]") name " "
      lower = Util.toLower alphaOnly
  in lower

setLegacy :: PvpChargedMove -> PvpChargedMove
setLegacy this =
  this { isLegacy = True }
