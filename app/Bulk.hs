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
  league  :: League,
  species :: String,
  attack  :: Int,
  defense :: Int,
  stamina :: Int
}

data League = Great | Ultra | Master | Peewee
  deriving Eq

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLeague <*> optSpecies
        <*> optAttack <*> optDefense <*> optStamina
      optLeague =
            O.flag' Great (
              O.short 'g' <>
              O.long "great" <>
              O.help "great league")
        <|> O.flag' Ultra (
              O.short 'u' <>
              O.long "ultra" <>
              O.help "ultra league")
        <|> O.flag' Master (
              O.short 'm' <>
              O.long "master" <>
              O.help "master league")
        <|> O.flag' Peewee (
              O.short 'p' <>
              O.long "peewee" <>
              O.help "peewee league")
        <|> pure Great
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      optAttack = O.argument O.auto (O.metavar "ATTTACK")
      optDefense = O.argument O.auto (O.metavar "DEFENSE")
      optStamina = O.argument O.auto (O.metavar "STAMINA")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Calculate level and bulk for a PVP pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

leaguePred :: League -> (Int -> Bool)
leaguePred league =
  case league of
    Great -> (<= 1500)
    Ultra -> (<= 2500)
    Master -> const True
    Peewee -> (<= 10)

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
          pred = leaguePred $ league options
          ivs = lastWhere (pred . Calc.cp gameMaster base) allIVs
          level = IVs.level ivs
          bulkForLevel = bulk gameMaster base ivs
          totalForLevel = total gameMaster base ivs *
            if (league options) == Peewee then 1000 else 1
      Printf.printf "%-4s %.2f %.2f\n" (PokeUtil.levelToString level)
        bulkForLevel totalForLevel
    )
    $ Exit.die

lastWhere :: (a -> Bool) -> [a] -> a
lastWhere pred =
  last . takeWhile pred

bulk :: GameMaster -> PokemonBase -> IVs -> Float
bulk gameMaster base ivs =
  let (level, _, defense, stamina) = IVs.getAll ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      defense' = fromIntegral $ PokemonBase.defense base + defense
      stamina' = fromIntegral $ PokemonBase.stamina base + stamina
  in cpMultiplier * sqrt (defense' * stamina')

total :: GameMaster -> PokemonBase -> IVs -> Float
total gameMaster base ivs =
  let (level, attack, defense, stamina) = IVs.getAll ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      attack' = fromIntegral $ PokemonBase.attack base + attack
      defense' = fromIntegral $ PokemonBase.defense base + defense
      stamina' = fromIntegral $ PokemonBase.stamina base + stamina
  in (cpMultiplier * attack') * (cpMultiplier * defense') *
       (cpMultiplier * stamina') / 100000
