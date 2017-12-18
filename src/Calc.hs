module Calc (
  cp,
  hp,
) where

import qualified GameMaster
import           GameMaster (GameMaster)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Stats
import           Stats (Stats)

cp :: GameMaster -> PokemonBase -> Stats -> Int
cp gameMaster pokemonBase stats =
  let (level, attack, defense, stamina) = Stats.getAll stats
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
  in floor $
    fromIntegral (PokemonBase.attack pokemonBase + attack) * cpMultiplier *
    sqrt (fromIntegral (PokemonBase.defense pokemonBase + defense)) *
    sqrt (fromIntegral (PokemonBase.stamina pokemonBase + stamina)) *
    cpMultiplier / 10

hp :: GameMaster -> PokemonBase -> Stats -> Int
hp gameMaster pokemonBase stats =
  let (level, attack, defense, stamina) = Stats.getAll stats
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
  in floor $
    fromIntegral (PokemonBase.stamina pokemonBase + stamina) * cpMultiplier
