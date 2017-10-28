module Move (
  Move (Move),
  moveType,
  power,
  duration,
  energy,
  isCharge,
  isQuick,
  stabFor,
  effectivenessAgainst,
  name
) where

import qualified Type
import           Type (Type)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Text.Regex as Regex

data Move = Move {
  movementId :: String,
  moveType :: Type,
  power    :: Float,
  duration :: Float,
  energy   :: Float
} deriving (Eq, Show)

isCharge :: Move -> Bool
isCharge this =
  not $ Move.isQuick this

isQuick :: Move -> Bool
isQuick this =
  Data.List.isSuffixOf "_FAST" $ movementId this

stabFor :: Move -> [Type] -> Float
stabFor this attackerTypes =
  Type.stabFor (Move.moveType this) attackerTypes

effectivenessAgainst :: Move -> [Type] -> Float
effectivenessAgainst this defenderTypes =
  Type.effectivenessAgainst (Move.moveType this) defenderTypes

name :: Move -> String
name this =
  let noFast = Regex.subRegex (Regex.mkRegex "_FAST$") (movementId this) ""
      alphaOnly = Regex.subRegex (Regex.mkRegex "[^a-zA-Z]") noFast " "
  in map Char.toLower alphaOnly
