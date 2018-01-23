module Calc (
  cp,
  hp,
) where

import qualified GameMaster
import           GameMaster (GameMaster)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified IVs
import           IVs (IVs)

cp :: GameMaster -> PokemonBase -> IVs -> Int
cp gameMaster pokemonBase ivs =
  let (level, attack, defense, stamina) = IVs.getAll ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
  in floor $
    fromIntegral (PokemonBase.attack pokemonBase + attack) * cpMultiplier *
    sqrt (fromIntegral (PokemonBase.defense pokemonBase + defense)) *
    sqrt (fromIntegral (PokemonBase.stamina pokemonBase + stamina)) *
    cpMultiplier / 10

hp :: GameMaster -> PokemonBase -> IVs -> Int
hp gameMaster pokemonBase ivs =
  let (level, attack, defense, stamina) = IVs.getAll ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
  in floor $
    fromIntegral (PokemonBase.stamina pokemonBase + stamina) * cpMultiplier
