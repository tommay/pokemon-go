module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Discounts
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil
import qualified Powerups

import           Control.Monad (join)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

import qualified Debug

data Options = Options {
  species :: String,
  cp      :: Int
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optSpecies <*> optCp
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      optCp = O.argument O.auto (O.metavar "CP")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Show possible level and IVs for a pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

-- This is by far the ugliest Haskell code I've ever written.  Maybe I
-- can make it nicer.
--
main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      let species = Main.species options
          cp = Main.cp options
      base <- GameMaster.getPokemonBase gameMaster species
      let allLevels = GameMaster.allLevels gameMaster
          allIVs = [IVs.new level attack defense stamina |
            let ivs = [0..15],
            level <- allLevels, attack <- ivs, defense <- ivs, stamina <- ivs]
          possible = filter ((== cp) . Calc.cp gameMaster base) allIVs
      mapM_ (\ ivs ->
        Printf.printf "%s: %d/%d/%d\n"
          (PokeUtil.levelToString $ IVs.level ivs)
          (IVs.attack ivs)
          (IVs.defense ivs)
          (IVs.stamina ivs))
        possible
    )
    $ Exit.die
