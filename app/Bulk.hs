module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
--import           Data.List ((!?))
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
import qualified Util

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
  allPowerups :: Bool,
  isShadow :: Bool,
  isPurified :: Bool,
  isLucky :: Bool,
  isTraded :: Bool,
  maybeMaxCandy :: Maybe Int,
  maybeMaxXlCandy :: Maybe Int,
  useBestBuddyBoost :: Bool,
  species :: String,
  maybeEvolutionSpeciesOrNumber :: Maybe SpeciesOrNumber,
  levelOrCp :: LevelOrCp,
  maybeCombatSpecies :: Maybe String,
  attack  :: Int,
  defense :: Int,
  stamina :: Int
}

data SpeciesOrNumber = Species String | Number Int
  deriving (Show)

data LevelOrCp = Level Float | Cp Int
  deriving (Show)

data League = Little | Great | Ultra | Master | Peewee
  deriving (Eq, Show)

-- XXX This is supposed to be in Data.List
(!?) :: [a] -> Int -> Maybe a
(!?) lst idx =
  case lst of
    [] -> Nothing
    (a:as) ->
      if idx == 0
        then Just a
        else as !? (idx - 1)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLeague
        <*> optOneLine <*> optSummary <*> optAllPowerups
        <*> optIsShadow <*> optIsPurified <*> optIsLucky <*> optIsTraded
        <*> optMaxCandy <*> optMaxXlCandy <*> optUseBestBuddyBoost
        <*> optSpecies <*> optMaybeEvolutionSpeciesOrNumber
        <*> optLevelOrCp <*> optMaybeCombatSpecies
        <*> optAttack <*> optDefense <*> optStamina
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
        <> O.short 'z'
        <> O.help "Output only the final level")
      optSummary = O.switch
        (  O.long "summary"
        <> O.short 's'
        <> O.help "Show a single summary line")
      optAllPowerups = O.switch
        (  O.long "all"
        <> O.short 'a'
        <> O.help "Show all powerups below the league cap")
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
      optMaxCandy = O.optional $ O.option O.auto
        (  O.long "candy"
        <> O.short 'c'
        <> O.metavar "CANDY"
        <> O.help "Use up to CANDY candy to power up the pokemon")
      optMaxXlCandy = O.optional $ O.option O.auto
        (  O.long "xlcandy"
        <> O.short 'x'
        <> O.metavar "XLCANDY"
        <> O.help "Use up to XLCANDY xlCandy to power up the pokemon")
      optUseBestBuddyBoost = O.switch
        (  O.long "buddy"
        <> O.short 'b'
        <> O.help "Use best buddy boost")
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      -- XXX There are two things about this that I don't quite understand:
      -- 1) Why is "Level <$>" the right thing to use here?
      --   A: O.option returns Parser a.  <$>/fmap will "reach into" the Parser
      --   and turn the "a" into a LevelOrCp.
      -- 2) The "-l" option is only recognized before the ATTACK argument
      -- is given?  That's not surprising; if "-l LEVEL" is given before
      -- CP could be then CP is skipped and we move right on to ATTACK,
      -- but how does the argument parsing actually work?
      optMaybeEvolutionSpeciesOrNumber = O.optional $
        let optEvolutionSpecies = Species <$> O.strOption
              (  O.long "evolution"
              <> O.short 'e'
              <> O.metavar "EVOLUTION"
              <> O.help "Evolution for PVP")
            optEvolutionNumber = Number <$> (
                    O.flag' 1 (
                    O.short '1' <>
                    O.help "first evolution")
              <|> O.flag' 2 (
                    O.short '2' <>
                    O.help "second evolution")
              <|> O.flag' 3 (
                    O.short '3' <>
                    O.help "third evolution"))
        in optEvolutionSpecies <|> optEvolutionNumber
      optLevelOrCp =
        let optLevel = Level <$> (O.option O.auto
              (  O.long "level"
              <> O.short 'l'
              <> O.metavar "LEVEL"
              <> O.help "Pokemon level"))
            optCp = Cp <$> (O.argument O.auto (O.metavar "CP"))
        in optLevel <|> optCp
      optMaybeCombatSpecies = O.optional $ O.strOption
        (  O.long "combat"
        <> O.short 'C'
        <> O.metavar "COMBAT-SPECIES"
        <> O.help "Species to combat as")
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
 
      base <- GameMaster.getPokemonBase gameMaster species

      let allLevels = GameMaster.powerUpLevels gameMaster
          makeIVs level = IVs.new level
            (attack options) (defense options) (stamina options)
          allIVs = map makeIVs allLevels
          calcCpForIVs = Calc.cp gameMaster
      level <- do
        case levelOrCp options of
          Level level -> return level
          Cp cp ->
            case List.find ((== cp) . calcCpForIVs base) allIVs of
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
              then PokemonBase.purificationCost base
              else (0, 0)
          (thirdMoveStardust, thirdMoveCandy) =
            -- The third move is always added after purification if applicable.
            let scale num dem = (`div` dem) . (* num)
                both func (a, b) = (func a, func b)
            in (both $ if isPurified
                 then scale 4 5
                 else if isShadow
                   then scale 6 5
                   else id) $
                 PokemonBase.thirdMoveCost base

      -- If no evolution target is given then for --little don't evolve else
      -- evolve fully.  Otherwise evolve to the given evolution or to the
      -- first, second, or third evolution.

      maybeTargetSpecies <- do
        case maybeEvolutionSpeciesOrNumber options of
          Nothing ->
            if league options == Little
              then pure $ Just species
              else pure Nothing
          Just (Species species) -> pure $ Just species
          Just (Number number) -> do
            evolutionChains <-
              PokeUtil.evolutionChains gameMaster False (species, 0)
            -- Suppose we have the evolutionChains for poliwag.  There are
            -- two: [poliwag, poliwhirl, poliwrath] and [poliwag, poliwhirl,
            -- politoed].  We want to be able to slice this into all the first
            -- evolutions, the second evolutions, and third evolutions:
            -- [poliwag, poliwag], [poliwhirl, poliwhirl], and [poliwrath,
            -- politoed].  That's what transpose does.  Then we use nub to get
            -- the unique evolutions for each stage.
            let evolutionStages = List.transpose evolutionChains
            case evolutionStages !? (number - 1) of
              Nothing -> Epic.fail $
                Printf.printf "%s does not have %d evolutions" species number
              Just evolutionsAtStage ->
                let evolutions = List.nub $ map fst evolutionsAtStage
                in case evolutions of
                  [] -> Epic.fail $
                    Printf.printf "%s has no evolutions?" species
                  [evolvedSpecies] -> pure $ Just evolvedSpecies
                  _ -> Epic.fail $
                    Printf.printf "%s has multiple evolutions: %s" species
                      (Util.toLower $ Util.commaSeparated "and" evolutions)

      (speciesEvolved, evolveCandy) <-
        PokeUtil.evolveSpeciesFullyWithCandy
          gameMaster (isTraded options) maybeTargetSpecies species

      -- XXX this could formerly be shadow or purified.
      baseEvolved <- GameMaster.getPokemonBase gameMaster speciesEvolved

      -- The result of the mapM is a m (Maybe pokemonBase).  Use "<-"
      -- to "get it out" of the monad.

      baseForCombat <- Maybe.fromMaybe baseEvolved <$>
        (mapM (GameMaster.getPokemonBase gameMaster) $
          Main.maybeCombatSpecies options)

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
            then min (iv + 2) 15
            else iv
          makePureIVs level = IVs.new level
            (purifyIv $ attack options)
            (purifyIv $ defense options)
            (purifyIv $ stamina options)
          allPureIVs = map makePureIVs allLevels
          pred = leaguePred $ league options
          powerUpIVs = Util.lastWhere (pred . calcCpForIVs baseEvolved)
            allPureIVs
          powerUpLevel = IVs.level powerUpIVs
          levelsAndCosts =
            filter ((`leMaybe` (maybeMaxCandy options)) . Cost.candy . snd) $
            filter ((`leMaybe` (maybeMaxXlCandy options)) . Cost.xlCandy . snd) $
            filter ((<= powerUpLevel) . fst) $
            Powerups.levelsAndCosts gameMaster discounts level
          getRank' = getRank gameMaster (pred . calcCpForIVs baseEvolved)
            powerUpIVs
          (statProductRank, statProductPercentile) = getRank'
            (getStatProduct gameMaster baseForCombat)
          (attackRank, attackPercentile) = getRank'
            (getAttack gameMaster baseForCombat)
          makeOutputString (level, cost) =
            let ivs = makePureIVs level
                (attackForLevel, defenseForLevel, totalForLevel) =
                  total gameMaster baseForCombat ivs
            in Printf.printf "%6d/%-7s: %-4s %.2f  %.2f %.2f"
                 (basePvpStardust + Cost.dust cost)
                 (candyToString (basePvpCandy + Cost.candy cost)
                   (Cost.xlCandy cost))
                 (PokeUtil.levelToString level)
                 (totalForLevel *
                   if (league options) == Peewee then 1000 else 1)
                 attackForLevel
                 defenseForLevel
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

leMaybe :: Ord a => a -> Maybe a -> Bool
leMaybe a =
  Maybe.maybe True (a <=)

total :: GameMaster -> PokemonBase -> IVs -> (Float, Float, Float)
total gameMaster base ivs =
  let (level, attack, defense, stamina) = IVs.getAll ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      attack' = fromIntegral $ PokemonBase.attack base + attack
      defense' = fromIntegral $ PokemonBase.defense base + defense
      stamina' = fromIntegral $ PokemonBase.stamina base + stamina
      attackForLevel = attack' * cpMultiplier
      defenseForLevel = defense' * cpMultiplier
      hpForLevel = fromIntegral $ floor $ stamina' * cpMultiplier
      statProduct = attackForLevel * defenseForLevel * hpForLevel
  in (attackForLevel, defenseForLevel, statProduct / 100000)

getStatProduct :: GameMaster -> PokemonBase -> IVs -> Float
getStatProduct gameMaster pokemonBase ivs =
  let (_, _, sp) = total gameMaster pokemonBase ivs
  in sp

getAttack :: GameMaster -> PokemonBase -> IVs -> Float
getAttack gameMaster pokemonBase ivs =
  let (a, _, _) = total gameMaster pokemonBase ivs
  in a

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
      metricsForAllIVs = map getMetricForIVs powerupIVs
      metricForIVs = getMetricForIVs ivs
      numberBetter = length $ filter (> metricForIVs) metricsForAllIVs
      rank = numberBetter + 1
      numberWorse = length $ filter (< metricForIVs) metricsForAllIVs
      percentile =  fromIntegral numberWorse / fromIntegral (length allIVs)
        * 100
  in (rank, percentile)

-- Given a league predicate and atttack/defense/stamina, determine the
-- powerup level and create an IVs for it.
--
getPowerupIVs :: GameMaster -> (IVs -> Bool) -> (Int, Int, Int) -> IVs
getPowerupIVs gameMaster areIVsOkForLeague (attack, defense, stamina) =
  let allLevels = GameMaster.powerUpLevels gameMaster
      allIVs = map (\ level -> IVs.new level attack defense stamina) allLevels
  in last $ filter areIVsOkForLeague allIVs
