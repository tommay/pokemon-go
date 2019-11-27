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
  league  :: League,
  oneLine :: Bool,
  isShadow :: Bool,
  species :: String,
  maybeEvolution :: Maybe String,
  cp      :: Int,
  attack  :: Int,
  defense :: Int,
  stamina :: Int
}

data League = Great | Ultra | Master | Peewee
  deriving (Eq, Show)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLeague <*> optOneLine <*> optIsShadow
        <*> optSpecies <*> optEvolution
        <*> optCp <*> optAttack <*> optDefense <*> optStamina
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
      optOneLine = O.switch
        (  O.long "one"
        <> O.short '1'
        <> O.help "Output only the final level")
      optIsShadow = O.switch
        (  O.long "shadow"
        <> O.short 's'
        <> O.help "Compute for shadow pokemon")
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      optEvolution = O.optional $ O.strOption
        (  O.long "evolution"
        <> O.short 'e'
        <> O.metavar "EVOLUTION"
        <> O.help "Evolution for PVP")
      optCp = O.argument O.auto (O.metavar "CP")
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

-- This is by far the ugliest Haskell code I've ever written.  Maybe I
-- can make it nicer.
--
main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      let species = Main.species options
      base <- GameMaster.getPokemonBase gameMaster species
      let allLevels = GameMaster.allLevels gameMaster
          makeIVs level = IVs.new level
            (attack options) (defense options) (stamina options)
          allIVs = map makeIVs allLevels
          calcCpForIvs = Calc.cp gameMaster
      level <- do
        case List.find ((== cp options) . calcCpForIvs base) allIVs of
          Just ivs -> return $ IVs.level ivs
          Nothing -> Epic.fail "no possible level for cp and ivs"
      level <- do return $ if isShadow options then 25 else level
      (evolvedSpecies, evolveCandy) <- PokeUtil.evolveSpeciesFullyWithCandy
        gameMaster (maybeEvolution options) species
      evolvedBase <- GameMaster.getPokemonBase gameMaster evolvedSpecies
      let purifyIv iv = if isShadow options
            then List.minimum [iv + 2, 15]
            else iv
          makePureIVs level = IVs.new level
            (purifyIv $ attack options)
            (purifyIv $ defense options)
            (purifyIv $ stamina options)
          allPureIVs = map makePureIVs allLevels
          pred = leaguePred $ league options
          powerUpLevel = IVs.level $
            lastWhere (pred . calcCpForIvs evolvedBase) allPureIVs
          levelsAndCosts = filter (\ (lvl, _, _) -> lvl <= powerUpLevel) $
            Powerups.levelsAndCosts gameMaster Discounts.noDiscounts level
          makeOutputString (level, dust, candy) =
            let ivs = makePureIVs level
                bulkForLevel = bulk gameMaster evolvedBase ivs
                totalForLevel = total gameMaster evolvedBase ivs *
                  if (league options) == Peewee then 1000 else 1
            in Printf.printf "%5d/%-4d: %-4s %.2f %.2f"
                 dust
                 (evolveCandy + candy)
                 (PokeUtil.levelToString level)
                 bulkForLevel
                 totalForLevel
      case levelsAndCosts of
        [] -> putStrLn $
          "CP is too high for " ++ show (league options) ++ " league"
        _ -> mapM_ (putStrLn . makeOutputString) $ if oneLine options
               then [last levelsAndCosts]
               else levelsAndCosts
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
