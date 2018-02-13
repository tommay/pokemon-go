module PokeUtil (
  addStats,
  setLevel,
  levelToString,
) where

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified MakePokemon
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Stats
import           Stats (Stats)

import qualified Text.Printf as Printf

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

setLevel :: GameMaster -> Float -> Pokemon -> Pokemon
setLevel gameMaster level pokemon =
  let ivs = IVs.setLevel (Pokemon.ivs pokemon) level
  in MakePokemon.makeForWhatever
       gameMaster
       ivs
       (Pokemon.pname pokemon)
       (Pokemon.base pokemon)
       (Pokemon.quick pokemon)
       (Pokemon.charge pokemon)

levelToString :: Float -> String
levelToString level =
  if fromIntegral (floor level) == level
    then show $ floor level
    else Printf.printf "%.1f" level
