module Main where

import qualified Epic
import qualified MyPokemon

main :: IO ()
main =
  Epic.catch (
    do
      ioLoaded <- MyPokemon.load "my_pokemon.yaml"
      myPokemon <- ioLoaded
      mapM_ print myPokemon)
    (\ex -> putStrLn $ "oops: " ++ ex)
