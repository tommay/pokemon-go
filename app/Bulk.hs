module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Cost
import           Cost (Cost)
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
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
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
  isTraded :: Bool,
  maybeMaxXlCandy :: Maybe Int,
  species :: String,
  maybeEvolution :: Maybe String,
  levelOrCp :: LevelOrCp,
  attack  :: Int,
  defense :: Int,
  stamina :: Int
}

data LevelOrCp = Level Float | Cp Int
  deriving (Show)

data League = Little | Great | Ultra | Master | Peewee
  deriving (Eq, Show)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLeague <*> optOneLine
        <*> optSummary
        <*> optIsShadow <*> optIsPurified <*> optIsLucky <*> optIsTraded
        <*> optMaxXlCandy
        <*> optSpecies <*> optEvolution
        <*> optLevelOrCp <*> optAttack <*> optDefense <*> optStamina
      optLeague =
            O.flag' Little (
              -- There is no short option because the obvious "-l" conflicts
              -- with the short option for "--level".
              O.long "little" <>
              O.help "little league")
        <|> O.flag' Great (
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
      optIsTraded = O.switch
        (  O.long "traded"
        <> O.short 'T'
        <> O.help "Pokemon has been traded, evolution may be free")
      optMaxXlCandy = O.optional $ O.option O.auto
        (  O.long "xlcandy"
        <> O.short 'x'
        <> O.metavar "XLCANDY"
        <> O.help "Use up to XLCANDY xlCandy to power up the pokemon")
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      optEvolution = O.optional $ O.strOption
        (  O.long "evolution"
        <> O.short 'e'
        <> O.metavar "EVOLUTION"
        <> O.help "Evolution for PVP")
      -- XXX There are two things about this that I don't quite understand:
      -- 1) Why is "Level <$>" the right thing to use here?
      --   A: O.option returns Parser a.  <$>/fmap will "reach into" the Parser
      --   and turn the "a" into a LevelOrCp.
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
    Little -> (<= 500)
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
      gameMaster <- join $ GameMaster.load
      let species = Main.species options
          isShadow = Main.isShadow options
          isPurified = Main.isPurified options
          isLucky = Main.isLucky options
          needsPurification = isShadow && isPurified
          discounts = Discounts.new gameMaster
            (isShadow && not isPurified) isPurified isLucky
          appendEvolvedSuffix =
            if isPurified
              then (++ "_PURIFIED")
              else if isShadow
                then (++ "_SHADOW")
                else id

      -- baseCurrent can be used for all CP calculations and to get the
      -- purification cost.
 
      baseCurrent <- do
        let speciesCurrent =
              -- This is different from appendEvolvedSuffix because
              -- that uses the suffix before purification and this
              -- uses the suffix after purification.
              if isShadow
                then species ++ "_SHADOW"
                else if isPurified
                  then species ++ "_PURIFIED"
                  else species
        GameMaster.getPokemonBase gameMaster speciesCurrent

      let speciesToEvolve = appendEvolvedSuffix species
      -- baseToEvolve can be used to get the evolution and third move costs.
      baseToEvolve <- do
        GameMaster.getPokemonBase gameMaster speciesToEvolve

      let allLevels = GameMaster.powerUpLevels gameMaster
          makeIVs level = IVs.new level
            (attack options) (defense options) (stamina options)
          allIVs = map makeIVs allLevels
          calcCpForIVs = Calc.cp gameMaster
      level <- do
        case levelOrCp options of
          Level level -> return level
          Cp cp ->
            case List.find ((== cp) . calcCpForIVs baseCurrent) allIVs of
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

      -- Little league pokemon must be unevolved so just use the species
      -- as given.
      (speciesEvolved, evolveCandy) <- if league options == Little
            then return $ (speciesToEvolve, 0)
            else do
              let maybeTarget = appendEvolvedSuffix <$> maybeEvolution options
              PokeUtil.evolveSpeciesFullyWithCandy
                gameMaster (isTraded options) maybeTarget speciesToEvolve

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
          powerUpIVs = lastWhere (pred . calcCpForIVs baseEvolved) allPureIVs
          powerUpLevel = IVs.level powerUpIVs
          levelsAndCosts =
            filter (leMaybe (maybeMaxXlCandy options) . Cost.xlCandy . snd) $
            filter ((<= powerUpLevel) . fst) $
            Powerups.levelsAndCosts gameMaster discounts level
          getRank' = getRank gameMaster (pred . calcCpForIVs baseEvolved)
            powerUpIVs
          (statProductRank, statProductPercentile) = getRank'
            (getStatProduct gameMaster baseEvolved)
          (attackRank, attackPercentile) = getRank'
            (getAttack gameMaster baseEvolved)
          makeOutputString (level, cost) =
            let ivs = makePureIVs level
                (attackForLevel, totalForLevel) =
                  total gameMaster baseEvolved ivs
            in Printf.printf "%5d/%-7s: %-4s %.2f  %.2f"
                 (basePvpStardust + Cost.dust cost)
                 (candyToString (basePvpCandy + Cost.candy cost)
                   (Cost.xlCandy cost))
                 (PokeUtil.levelToString level)
                 (totalForLevel *
                   if (league options) == Peewee then 1000 else 1)
                 attackForLevel
      if not $ summary options
        then do
          Printf.printf "statProduct rank %d, >%.2f%%\n"
            statProductRank statProductPercentile
          Printf.printf "attack rank %d, >%.2f%%\n" attackRank attackPercentile
        else pure ()
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
                rankString = Printf.printf "sp >%.2f%%, atk >%.2f%%"
                  statProductPercentile attackPercentile
             in Printf.printf "%-14s %s, %s\n" (inputString ++ ":")
               (outputString :: String) (rankString :: String)
          else mapM_ (putStrLn . makeOutputString) $ if oneLine options
            then [last levelsAndCosts]
            else levelsAndCosts
    )
    $ Exit.die

candyToString :: Int -> Int  -> String
candyToString candy xlCandy =
  Printf.printf "%d%s" candy
    (if xlCandy == 0
      then ""
      else Printf.printf "+%d" xlCandy)

leMaybe :: Ord a => Maybe a -> a -> Bool
leMaybe =
  Maybe.maybe (const True) (>=) 

lastWhere :: (a -> Bool) -> [a] -> a
lastWhere pred =
  last . filter pred

total :: GameMaster -> PokemonBase -> IVs -> (Float, Float)
total gameMaster base ivs =
  let (level, attack, defense, stamina) = IVs.getAll ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      attack' = fromIntegral $ PokemonBase.attack base + attack
      defense' = fromIntegral $ PokemonBase.defense base + defense
      stamina' = fromIntegral $ PokemonBase.stamina base + stamina
      attackForLevel = attack' * cpMultiplier
      hpForLevel = fromIntegral $ floor $ stamina' * cpMultiplier
      statProduct = attackForLevel * (defense' * cpMultiplier) * hpForLevel
  in (attackForLevel, statProduct / 100000)

getStatProduct :: GameMaster -> PokemonBase -> IVs -> Float
getStatProduct gameMaster pokemonBase ivs =
  snd $ total gameMaster pokemonBase ivs

getAttack :: GameMaster -> PokemonBase -> IVs -> Float
getAttack gameMaster pokemonBase ivs =
  fst $ total gameMaster pokemonBase ivs

-- The rank this determines matches the rank given by PokeGenie.  Which
-- is good for PokeGenie.
--
getRank :: GameMaster -> (IVs -> Bool) -> IVs -> (IVs -> Float) ->
  (Int, Float)
getRank gameMaster areIVsOkForLeague ivs getMetricForIVs =
  let allIVs = [(attack, defense, stamina) |
        let ivs = [0..15],
        attack <- ivs, defense <- ivs, stamina <- ivs]
      powerupIVs = map (getPowerupIVs gameMaster areIVsOkForLeague) allIVs
      allMetrics = map getMetricForIVs powerupIVs
      metric = getMetricForIVs ivs
      numberBetter = length $ filter (> metric) allMetrics
      rank = numberBetter + 1
      numberWorse = length $ filter (< metric) allMetrics
      percentile =  fromIntegral numberWorse / fromIntegral (length allIVs)
        * 100
  in (rank, percentile)

getPowerupIVs :: GameMaster -> (IVs -> Bool) -> (Int, Int, Int) -> IVs
getPowerupIVs gameMaster areIVsOkForLeague (attack, defense, stamina) =
  let allLevels = GameMaster.powerUpLevels gameMaster
      allIVs = map (\ level -> IVs.new level attack defense stamina) allLevels
  in last $ filter areIVsOkForLeague allIVs
