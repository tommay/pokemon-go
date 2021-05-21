-- Need to make PokemonBase an instance of Generic so it can be made
-- an instance of the appropriate serializable typeclass in
-- GameMaster.hs.
{-# LANGUAGE DeriveGeneric #-}

module PokemonBase (
  PokemonBase,
  new,
  pokemonId,
  species,
  types,
  attack,
  defense,
  stamina,
  evolutions,
  quickMoves,
  chargeMoves,
  hasEvolutions,
  baseCaptureRate,
  thirdMoveCost,
  purificationCost,
) where

import           Type (Type)
import qualified Move
import           Move (Move)
import           Rarity (Rarity)

import           GHC.Generics (Generic)

import           Data.Text (Text)

data PokemonBase = PokemonBase {
  pokemonId    :: String,
  species      :: String,
  types        :: [Type],
  attack       :: Int,
  defense      :: Int,
  stamina      :: Int,
    -- (species, candy, noCandyCostViaTrade):
  evolutions   :: [(String, Int, Bool)],
  quickMoves   :: [Move],
  chargeMoves  :: [Move],
  parent       :: Maybe String,
  baseCaptureRate :: Float,
  thirdMoveCost :: (Int, Int),  -- (stardust, candy)
  purificationCost :: (Int, Int),  -- (stardust, candy)
  rarity       :: Rarity
} deriving (Generic)

instance Show PokemonBase where
  show = species

new = PokemonBase

hasEvolutions :: PokemonBase -> Bool
hasEvolutions this = (not . null) $ PokemonBase.evolutions this
