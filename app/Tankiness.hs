-- Sort all pokemon in GAME_MASTER by tankiness, which is the ratio of how much
-- cp comes from defense and stamina vs. how much comes from attack, i.e.,
-- sqrt(defense*stamina) / attack.  High tankiness is a partial indicator
-- of how well the pokemon will do in pvp.
--
-- Additionaly, pokemon are filtered for a max cp of at least 1430 so they're
-- viable in great league.

module Main where

import qualified Calc
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified GameMaster
import qualified IVs
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Util

import           Control.Monad (join, forM_)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

main =
  Epic.catch (
    do
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      let allBases = GameMaster.allPokemonBases gameMaster
          pvpCapable = filter ((>= 1430) . maxCp gameMaster) allBases
          augmented = Util.augment tankiness pvpCapable
          sorted = List.reverse $ List.sortOn snd augmented
      forM_ sorted $ \ (base, tankiness) ->
        Printf.printf "%s %.1f\n"
          (PokemonBase.species base)
          tankiness
    )
    $ Exit.die

tankiness :: PokemonBase -> Float
tankiness base =
  (sqrt $ fromIntegral $ PokemonBase.defense base * PokemonBase.stamina base)
    / (fromIntegral $ PokemonBase.attack base)

maxCp :: GameMaster -> PokemonBase -> Int
maxCp gameMaster base =
  let maxIVs = IVs.new 40 15 15 15
  in Calc.cp gameMaster base maxIVs
