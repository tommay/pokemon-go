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
  summary :: Bool,
  isShadow :: Bool,
  isPurified :: Bool,
  isLucky :: Bool,
  species :: String,
  maybeEvolution :: Maybe String,
  levelOrCp :: LevelOrCp,
  attack  :: Int,
  defense :: Int,
  stamina :: Int
}

data LevelOrCp = Level Float | Cp Int

data League = Great | Ultra | Master | Peewee
  deriving (Eq, Show)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLeague <*> optOneLine
        <*> optSummary
        <*> optIsShadow <*> optIsPurified<*> optIsLucky
        <*> optSpecies <*> optEvolution
        <*> optLevelOrCp <*> optAttack <*> optDefense <*> optStamina
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
      optSummary = O.switch
        (  O.long "summary"
        <> O.short 's'
        <> O.help "Show a single summary line")
      optIsShadow = O.switch
        (  O.long "shadow"
        <> O.short 'S'
        <> O.help "Compute for shadow pokemon")
      optIsPurified = O.switch
        (  O.long "purified"
        <> O.short 'P'
        <> O.help "Compute for purified pokemon")
      optIsLucky = O.switch
        (  O.long "lucky"
        <> O.short 'L'
        <> O.help "Compute for lucky pokemon")
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      optEvolution = O.optional $ O.strOption
        (  O.long "evolution"
        <> O.short 'e'
        <> O.metavar "EVOLUTION"
        <> O.help "Evolution for PVP")
      -- XXX There are two things about this that I don't quite understand:
      -- 1) Why is "Level <$>" the right thing to use here?
      -- 2) The "-l" option is only recognized before the ATTACK argument
      -- is given?  That's not surprising; if "-l LEVEL" is given before
      -- CP could be then CP is skipped and we move right on to ATTACK,
      -- but how does the argument parsing actually work?
      optLevelOrCp =
        let optLevel = Level <$> (O.option O.auto
              (  O.long "level"
              <> O.short 'l'
              <> O.metavar "LEVEL"
              <> O.help "Pokemon level"))
            optCp = Cp <$> (O.argument O.auto (O.metavar "CP"))
        in optLevel <|> optCp
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
          isShadow' = isShadow options
          isPurified' = isPurified options
          isLucky' = isLucky options
          needsPurification = isShadow' && isPurified'
          discounts = Discounts.new gameMaster
            (isShadow' && not isPurified') isPurified' isLucky'
          appendEvolvedSuffix =
            if isPurified'
              then (++ "_PURIFIED")
              else if isShadow'
                then (++ "_SHADOW")
                else id

      -- baseCurrent can be used for all CP calculations and to get the
      -- purification cost.
      baseCurrent <- do
        let speciesCurrent =
              if isShadow'
                then species ++ "_SHADOW"
                else if isPurified'
                  then species ++ "_PURIFIED"
                  else species
        GameMaster.getPokemonBase gameMaster speciesCurrent

      let speciesToEvolve = appendEvolvedSuffix species
      -- baseToEvolve can be used to get the evolution and third move costs.
      baseToEvolve <- do
        GameMaster.getPokemonBase gameMaster speciesToEvolve

      let allLevels = GameMaster.allLevels gameMaster
          makeIVs level = IVs.new level
            (attack options) (defense options) (stamina options)
          allIVs = map makeIVs allLevels
          calcCpForIvs = Calc.cp gameMaster
      level <- do
        case levelOrCp options of
          Level level -> return level
          Cp cp ->
            case List.find ((== cp) . calcCpForIvs baseCurrent) allIVs of
              Just ivs -> return $ IVs.level ivs
              Nothing -> Epic.fail "no possible level for cp and ivs"
      -- If both isShadow and isPurified are true, it means the command line
      -- cp is for a shadow pokemon but the calculations should be for after
      -- it is purified and becomes level 25.
      level <- do
        return $ if needsPurification
          then 25
          else level
      let (purificationStardustNeeded, purificationCandyNeeded) =
            if needsPurification
              then PokemonBase.purificationCost baseCurrent
              else (0, 0)
          (thirdMoveStardust, thirdMoveCandy) =
            PokemonBase.thirdMoveCost baseToEvolve

      (speciesEvolved, evolveCandy) <- do
        let maybeTarget = appendEvolvedSuffix <$> maybeEvolution options
        PokeUtil.evolveSpeciesFullyWithCandy
          gameMaster maybeTarget speciesToEvolve
      baseEvolved <- GameMaster.getPokemonBase gameMaster speciesEvolved
      if not $ summary options
        then do
          if needsPurification
            then putStrLn $ Printf.printf "pure:   %d/%d"
              purificationStardustNeeded purificationCandyNeeded
            else pure ()
          if evolveCandy /= 0
            then putStrLn $ Printf.printf "evolve: /%d" evolveCandy
            else pure ()
          putStrLn $ Printf.printf "move:   %d/%d"
              thirdMoveStardust thirdMoveCandy
        else pure ()
      let basePvpStardust = purificationStardustNeeded + thirdMoveStardust
          basePvpCandy = purificationCandyNeeded + evolveCandy + thirdMoveCandy
          purifyIv iv = if needsPurification
            then List.minimum [iv + 2, 15]
            else iv
          makePureIVs level = IVs.new level
            (purifyIv $ attack options)
            (purifyIv $ defense options)
            (purifyIv $ stamina options)
          allPureIVs = map makePureIVs allLevels
          pred = leaguePred $ league options
          powerUpLevel = IVs.level $
            lastWhere (pred . calcCpForIvs baseEvolved) allPureIVs
          levelsAndCosts = filter (\ (lvl, _, _) -> lvl <= powerUpLevel) $
            Powerups.levelsAndCosts gameMaster discounts level
          makeOutputString (level, dust, candy) =
            let ivs = makePureIVs level
                (attackForLevel, totalForLevel) =
                  total gameMaster baseEvolved ivs
            in Printf.printf "%5d/%-4d: %-4s %.2f   %.2f"
                 (basePvpStardust + dust)
                 (basePvpCandy + candy)
                 (PokeUtil.levelToString level)
                 (totalForLevel *
                   if (league options) == Peewee then 1000 else 1)
                 attackForLevel
      case levelsAndCosts of
        [] -> putStrLn $
          Printf.printf "%s CP is too high for %s league"
          (PokemonBase.species baseEvolved) (show $ league options)
        _ -> if summary options
          then
            let levelOrCpString = case levelOrCp options of
                  Level level -> "-l " ++ (PokeUtil.levelToString level)
                  Cp cp -> show cp
                inputString = Printf.printf "%s %d %d %d" levelOrCpString
                  (attack options) (defense options) (stamina options)
                outputString = makeOutputString $ last levelsAndCosts
             in Printf.printf "%-14s %s\n" (inputString ++ ":")
               (outputString :: String)
          else mapM_ (putStrLn . makeOutputString) $ if oneLine options
            then [last levelsAndCosts]
            else levelsAndCosts
    )
    $ Exit.die

lastWhere :: (a -> Bool) -> [a] -> a
lastWhere pred =
  last . takeWhile pred

total :: GameMaster -> PokemonBase -> IVs -> (Float, Float)
total gameMaster base ivs =
  let (level, attack, defense, stamina) = IVs.getAll ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      attack' = fromIntegral $ PokemonBase.attack base + attack
      defense' = fromIntegral $ PokemonBase.defense base + defense
      stamina' = fromIntegral $ PokemonBase.stamina base + stamina
      attackForLevel = attack' * cpMultiplier
      statProduct = attackForLevel * (defense' * cpMultiplier) *
        (stamina' * cpMultiplier)
  in (attackForLevel, statProduct / 100000)
