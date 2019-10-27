module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil

import           Control.Monad (join)
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  stamina :: Int,
  cpm     :: Float,
  species :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optStamina <*> optCpm <*> optSpecies
      optStamina = O.argument O.auto (O.metavar "STAMINA")
      optCpm = O.argument O.auto (O.metavar "CPM")
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Calculate boss cp")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      base <- GameMaster.getPokemonBase gameMaster $ species options
      let cpMultiplier = cpm options
          cp =
            fromIntegral (PokemonBase.attack base + 15) * cpMultiplier
            * sqrt (fromIntegral $
              (PokemonBase.defense base + 15) * (stamina options))
            * cpMultiplier / 10
      Printf.printf "%f\n" cp
    )
    $ Exit.die

-- Multiplying by cpm once:
-- 0.9128 is to low for granbull
-- 0.9129 works for granbull
--        but is too big for gengar
-- 0.91286 works for gengar
-- too low for raich_alola
-- 0.91287 works for raichu_alola, gengar, granbull, sharpedo, skuntank

-- But it seems more likely that cpm gets twice, i.e., for attack and def*stamina

-- 0.9554423059504954 works for everything
-- 0.95545 works for everything

