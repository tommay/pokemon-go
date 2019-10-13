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

import           Control.Monad (join, forM_)
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  species :: String,
  attack  :: Int,
  defense :: Int,
  stamina :: Int
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optSpecies <*> optAttack <*> optDefense <*> optStamina
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      optAttack = O.argument O.auto (O.metavar "ATTTACK")
      optDefense = O.argument O.auto (O.metavar "DEFENSE")
      optStamina = O.argument O.auto (O.metavar "STAMINA")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Calculate bulk for a pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      base <- GameMaster.getPokemonBase gameMaster $ species options
      let allLevels = GameMaster.allLevels gameMaster
          makeIVs level = IVs.new level
            (attack options) (defense options) (stamina options)
          allIVs = map makeIVs allLevels
          ivs = lastWhere ((<= 1500) . Calc.cp gameMaster base) allIVs
          level = IVs.level ivs
          bulkForLevel = bulk gameMaster base ivs
      Printf.printf "%s %.2f\n" (PokeUtil.levelToString level) bulkForLevel
    )
    $ Exit.die

lastWhere :: (a -> Bool) -> [a] -> a
lastWhere pred list =
  let lastWhere' last [] = last
      lastWhere' last (a:as) =
        if pred a
          then lastWhere' a as
          else last
  in lastWhere' (error "No matching element") list

bulk :: GameMaster -> PokemonBase -> IVs -> Float
bulk gameMaster base ivs =
  let (level, _, defense, stamina) = IVs.getAll ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      defense' = fromIntegral $ PokemonBase.defense base + defense
      stamina' = fromIntegral $ PokemonBase.stamina base + stamina
  in cpMultiplier * sqrt (defense' * stamina')
