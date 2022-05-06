-- Need to make PokemonBase an instance of Generic so it can be made
-- an instance of the appropriate serializable typeclass in
-- GameMaster.hs.
{-# LANGUAGE DeriveGeneric #-}

module TempEvoOverride (
  TempEvoOverride (TempEvoOverride),
  tempEvoId,
  typeOverrides,
  attack,
  defense,
  stamina,
) where

import           GHC.Generics (Generic)

import           StringMap (StringMap)
import           Type (Type)

new = TempEvoOverride

data TempEvoOverride = TempEvoOverride {
  tempEvoId :: String,
  typeOverrides :: [Type],
  attack :: Int,
  defense :: Int,
  stamina :: Int
} deriving (Show, Generic)
