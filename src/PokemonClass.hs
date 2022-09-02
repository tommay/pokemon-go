-- Need to make PokemonClass an instance of Generic so it can be made an
-- instance of the appropriate serializable typeclass in
-- GameMaster.hs.
{-# LANGUAGE DeriveGeneric #-}

module PokemonClass (
  PokemonClass (..),
) where

import           GHC.Generics (Generic)

-- Only Legendary, Mythic, and UltraBeast occur in the gamemaster file.
-- I add Normal to make parsing and using the file easier.

data PokemonClass = Normal | Legendary | Mythic | UltraBeast
  deriving (Show, Generic)
