module Main where

import qualified MyPokemon

main :: IO ()
main = do
  result <- MyPokemon.load "my_pokemon.yaml"
  case result of
    Right myPokemon ->
      mapM_ print myPokemon
    Left exception ->
      print exception
