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
  isPurified :: Bool,
  isLucky :: Bool,
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
  let opts = Options <$> optLeague <*> optOneLine
        <*> optIsShadow <*> optIsPurified<*> optIsLucky
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
          isShadow' = isShadow options
          isPurified' = isPurified options
          isLucky' = isLucky options
          needsPurification = isShadow' && isPurified'
          discounts = Discounts.new
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
        case List.find ((== cp options) . calcCpForIvs baseCurrent) allIVs of
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
      if needsPurification
        then putStrLn $ Printf.printf "purification: %d/%d"
          purificationStardustNeeded purificationCandyNeeded
        else return ()
      if evolveCandy /= 0
        then putStrLn $ Printf.printf "evolve: /%d" evolveCandy
        else return ()
      putStrLn $ Printf.printf "third move: %d/%d"
          thirdMoveStardust thirdMoveCandy
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
                bulkForLevel = bulk gameMaster baseEvolved ivs
                totalForLevel = total gameMaster baseEvolved ivs *
                  if (league options) == Peewee then 1000 else 1
            in Printf.printf "%5d/%-4d: %-4s %.2f %.2f"
                 (basePvpStardust + dust)
                 (basePvpCandy + candy)
                 (PokeUtil.levelToString level)
                 bulkForLevel
                 totalForLevel
      case levelsAndCosts of
        [] -> putStrLn $
          Printf.printf "%s CP is too high for %s league"
          (PokemonBase.species baseEvolved) (show $ league options)
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
