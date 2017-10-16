module Calc (
  cp,
  hp,
) where

import qualified PokemonBase
import           PokemonBase (PokemonBase)

cp :: PokemonBase -> Float -> Integer -> Integer -> Integer -> Integer
cp pokemonBase cpMultiplier attack defense stamina =
  floor $
    fromIntegral (PokemonBase.attack pokemonBase + attack) * cpMultiplier *
    sqrt (fromIntegral (PokemonBase.defense pokemonBase + defense)) *
    sqrt (fromIntegral (PokemonBase.stamina pokemonBase + stamina)) *
    cpMultiplier / 10

hp :: PokemonBase -> Float -> Integer -> Integer
hp pokemonBase cpMultiplier stamina =
  floor $
    fromIntegral (PokemonBase.stamina pokemonBase + stamina) * cpMultiplier