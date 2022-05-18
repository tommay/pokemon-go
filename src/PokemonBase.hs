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
  rarity,
  isMega,
  makeTempEvos,
) where

import           Type (Type)
import           Move (Move)
import           Rarity (Rarity)
import qualified TempEvoOverride
import           TempEvoOverride (TempEvoOverride)
import qualified Util

import qualified Debug as D

import qualified Text.Regex as Regex

import           GHC.Generics (Generic)

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
  rarity       :: Rarity,
  tempEvoOverrides :: [TempEvoOverride],
  isMega       :: Bool
} deriving (Generic)

instance Show PokemonBase where
  show = species

new = PokemonBase

hasEvolutions :: PokemonBase -> Bool
hasEvolutions this = (not . null) $ PokemonBase.evolutions this

makeTempEvos :: PokemonBase -> [PokemonBase]
makeTempEvos this =
  map (applyTempEvoOverride this) $ tempEvoOverrides this

applyTempEvoOverride :: PokemonBase -> TempEvoOverride -> PokemonBase
applyTempEvoOverride this tempEvoOverride =
  this {
    species = makeTempEvoSpecies
      (species this) (TempEvoOverride.tempEvoId tempEvoOverride),
    types = TempEvoOverride.typeOverrides tempEvoOverride,
    attack = TempEvoOverride.attack tempEvoOverride,
    defense = TempEvoOverride.defense tempEvoOverride,
    stamina = TempEvoOverride.stamina tempEvoOverride,
    isMega = True
  }

makeTempEvoSpecies :: String -> String -> String
makeTempEvoSpecies baseSpecies tempEvoId =
  let regex = Regex.mkRegex "^TEMP_EVOLUTION_MEGA(_([A-Z]+))?$"
  in case Regex.matchRegex regex tempEvoId of
       Just [_, suffix] ->
         Util.toLower $ "mega_" ++ baseSpecies ++
           (if null suffix then "" else "_" ++ suffix)
       _ -> error $ "Unknown tempEvoId format: " ++ tempEvoId
