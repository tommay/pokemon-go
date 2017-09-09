import qualified System.Environment

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import           PokemonBase (PokemonBase)
import           Move (Move)
import           Type (Type)

main = do
  Epic.catch (
    do
      ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
      gameMaster <- ioGameMaster
      args <- System.Environment.getArgs
      species <- case args of
        [species] -> return species
        _ -> Epic.fail "usage: counter species"
      defenderBase <- GameMaster.getPokemonBase gameMaster species
      ioMyPokemon <- MyPokemon.load "my_pokemon.yaml"
      myPokemon <- ioMyPokemon
      pokemon <- mapM (makePokemon gameMaster) myPokemon
      return ())
    (\ex -> putStrLn $ "oops: " ++ (show ex))

data Pokemon = Pokemon {
  name        :: String,
  species     :: String,
  types       :: [Type],
  attack      :: Integer,
  defense     :: Integer,
  stamina     :: Integer,
  quick       :: Move,
  charge      :: Move,
  base        :: PokemonBase
} deriving (Show)

makePokemon :: Epic.MonadCatch m => GameMaster -> MyPokemon -> m Pokemon
makePokemon gameMaster myPokemon = do
  base <- GameMaster.getPokemonBase gameMaster $ MyPokemon.species myPokemon
  quick <- GameMaster.getMove gameMaster $ MyPokemon.quick myPokemon
  charge <- GameMaster.getMove gameMaster $ MyPokemon.charge myPokemon
  return $ Pokemon "my name" "my species" [] 0 0 0 quick charge base

{-
map counter myPokemon

counter :: GameMaster -> String -> MyPokemon -> Result
counter gameMaster species myPokemon =
-}
