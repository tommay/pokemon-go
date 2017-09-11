import qualified System.Environment

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified PokemonBase
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
  let name = MyPokemon.name myPokemon
      species = MyPokemon.species myPokemon
  let fromGameMaster getFunc keyFunc = getFunc gameMaster $ keyFunc myPokemon
  base <- fromGameMaster GameMaster.getPokemonBase MyPokemon.species
  let types = PokemonBase.types base
  quick <- fromGameMaster GameMaster.getQuick MyPokemon.quickName
  charge <- fromGameMaster GameMaster.getCharge MyPokemon.chargeName
  _attack <- MyPokemon.attack myPokemon
  let attack = _attack + PokemonBase.attack base
  _defense <- MyPokemon.defense myPokemon
  let defense = _defense + PokemonBase.defense base
  _stamina <- MyPokemon.stamina myPokemon
  let stamina = _stamina + PokemonBase.stamina base
  return $ Pokemon name species types attack defense stamina quick charge base

{-
map counter myPokemon

counter :: GameMaster -> String -> MyPokemon -> Result
counter gameMaster species myPokemon =
-}
