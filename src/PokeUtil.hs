module PokeUtil (
  addStats,
) where

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Stats
import           Stats (Stats)

addStats :: Epic.MonadCatch m => GameMaster -> MyPokemon -> m MyPokemon
addStats gameMaster myPokemon =
  case MyPokemon.ivs myPokemon of
    Nothing -> return myPokemon
    Just ivs -> do
      pokemonBase <- GameMaster.getPokemonBase gameMaster $
        MyPokemon.species myPokemon
      return $ MyPokemon.setStats myPokemon $
        map (makeStats gameMaster pokemonBase) ivs

makeStats :: GameMaster -> PokemonBase -> IVs -> Stats
makeStats gameMaster pokemonBase ivs =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster $ IVs.level ivs
      makeStat getBaseStat getIvStat =
        (fromIntegral $ getBaseStat pokemonBase + getIvStat ivs) * cpMultiplier
  in Stats.new
       (makeStat PokemonBase.attack IVs.attack)
       (makeStat PokemonBase.defense IVs.defense)
       (floor (makeStat PokemonBase.stamina IVs.stamina))
