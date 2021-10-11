-- Need to make Move an instance of Generic so it can be made an
-- instance of the appropriate serializable typeclass in
-- GameMaster.hs.
{-# LANGUAGE DeriveGeneric #-}

module Rarity (
  Rarity (..),
) where

import           GHC.Generics (Generic)

-- Only Legendary and Mythic occur in the gamemaster file.  I add Normal to
-- make parsing and using the file easier.

data Rarity = Normal | Legendary | Mythic
  deriving (Show, Generic)
