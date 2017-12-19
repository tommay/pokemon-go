module Main where

import qualified Epic
import qualified MyPokemon

import           Control.Monad (join)

main :: IO ()
main =
  Epic.catch (
    do
      myPokemon <- join $ MyPokemon.load "my_pokemon.yaml"
      mapM_ print myPokemon)
    (\ex -> putStrLn $ "oops: " ++ ex)
