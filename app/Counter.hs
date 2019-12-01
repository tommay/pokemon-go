{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import           Battle (Battle)
import qualified BattlerUtil
import           BattlerUtil (Battler, Level (Normal))
import qualified Breakpoint
import qualified Discounts
import qualified Epic
import qualified Friend
import           Friend (Friend)
import qualified IVs
import           IVs (IVs)
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import           Move (Move)
import qualified MakePokemon
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Mythical
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil
import qualified Powerups
import qualified TweakLevel
import qualified Type
import           Type (Type)
import qualified Weather
import           Weather (Weather (..))

import qualified Debug

import           Control.Monad (join, forM, forM_)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified System.Exit as Exit
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  maybeWeather :: Maybe Weather,
  maybeFriend :: Maybe Friend,
  sortOutputBy :: SortOutputBy,
  dpsFilter :: Maybe Int,
  top      :: Maybe Int,
  tweakLevel :: Float -> Float,
  legendary :: Bool,
  attackerSource :: AttackerSource,
  showBreakpoints :: Bool,
  showPowerups :: Bool,
  maybeMaxCandy :: Maybe Int,
  maybeMaxDust  :: Maybe Int,
  raidGroup :: Bool,
  showAllMovesets :: Bool,
  defender :: Battler
}

data SortOutputBy =
  ByDamage | ByDps | ByProduct | ByDamagePerHp | ByTimeToFaint | Weighted Float
  deriving (Show)

data AttackerSource =
    FromFiles [FilePath]
  | AllAttackers
  | MovesetFor [Battler]
  deriving (Show)

data Result = Result {
  pokemon   :: Pokemon,
  pokemonVariants :: [Pokemon],
  minDps    :: Float,
  minDamage :: Int,
  timeToFaint :: Float
} deriving (Show)

defaultFilename = "my_pokemon.yaml"
defaultAttackerLevel = IVs.level IVs.defaultIVs

getOptions :: IO Options
getOptions =
  let opts = Options <$> optWeather <*> optFriend <*>
        optSortOutputBy <*> optDpsFilter <*>
        optTop <*>
        optTweakLevel <*> optLegendary <*> optAttackerSource <*>
        optShowBreakpoints <*>
        optShowPowerups <*> optMaxCandy <*> optMaxDust <*>
        optRaidGroup <*> optShowMoveset <*>
        optDefender
      optWeather = O.optional Weather.optWeather
      optFriend = O.optional Friend.optFriend
      optSortOutputBy =
        let optGlass = O.flag' ByDps
              (  O.long "glass"
              <> O.short 'g'
              <> O.help "Sort output by dps to find glass cannons")
            optProduct = O.flag' ByProduct
              (  O.long "product"
              <> O.short 'p'
              <> O.help "Sort output by product of dps and damage")
            optDamagePerHp = O.flag' ByDamagePerHp
              (  O.short 'h'
              <> O.help "Sort output by damage per hp")
            optTimeToFaint = O.flag' ByTimeToFaint
              (  O.short 'F'
              <> O.help "Sort output by time to faint")
            optWeighted = Weighted <$> O.option O.auto
              (  O.long "weighted"
              <> O.short 'w'
              <> O.metavar "WEIGHT"
              <> O.help ("Sort by weighted mix of dps and damage, " ++
                   "where weight varies from 0.0 for pure dps to " ++
                   "1.0 for pure damage"))
        in optGlass <|> optProduct <|> optDamagePerHp <|> optTimeToFaint
             <|> optWeighted <|> pure ByDamage
      optDpsFilter = O.optional $ O.option O.auto
        (  O.long "dps"
        <> O.short 'd'
        <> O.metavar "N"
        <> O.help "Filter pokemon to the top 1/N by DPS")
      optTop = O.optional $ O.option O.auto
        (  O.long "top"
        <> O.short 't'
        <> O.metavar "N"
        <> O.help "Show the top N attacker species")
      optTweakLevel = TweakLevel.optTweakLevel 
      optLegendary = O.flag True False
        (  O.long "legendary"
        <> O.short 'L'
        <> O.help "Exclude legendaries when using -a")
      optAttackerSource =
        let optFilenames = FromFiles <$> (O.some . O.strOption)
              (  O.long "file"
              <> O.short 'f'
              <> O.metavar "FILE"
              <> O.help ("File to read my_pokemon from (defauult: " ++
                   defaultFilename ++ ")"))
            optAll = O.flag' AllAttackers
              (  O.long "all"
              <> O.short 'a'
              <> O.help "Consider all pokemon, not just the ones in FILE")
            optMovesetFor = MovesetFor <$>
              (O.some . O.option
                (BattlerUtil.optParseBattler IVs.defaultIVs))
              (  O.long "moveset"
              <> O.short 'm'
              <> O.metavar "ATTACKER[:LEVEL]"
              <> O.help "Rate the movesets for ATTACKER against DEFENDER")
        in optFilenames <|> optMovesetFor <|> optAll
             <|> (pure $ FromFiles [defaultFilename])
      optShowBreakpoints = O.switch
        (  O.long "breakpoints"
        <> O.short 'B'
        <> O.help "Show attacker breakpoints")
      optShowPowerups = O.switch
        (  O.long "powerups"
        <> O.short 'P'
        <> O.help "Show attacker with all powerups")
      optMaxCandy = O.optional $ O.option O.auto
        (  O.long "candy"
        <> O.short 'C'
        <> O.metavar "CANDY"
        <> O.help "Use up to CANDY candy to power up the pokemon")
      optMaxDust = O.optional $ O.option O.auto
        (  O.long "stardust"
        <> O.short 'S'
        <> O.metavar "STARDUST"
        <> O.help "Use up to STARDUST starust to power up the pokemon")
      optRaidGroup = O.switch
        (  O.long "raidgroup"
        <> O.short 'R'
        <> O.help "Defender is facing a raid group and always has energy for a charge move")
      optShowMoveset = O.switch
        (  O.short 'M'
        <> O.help "Show movesets for -a")
      optDefender = O.argument
        (BattlerUtil.optParseBattler IVs.defaultIVs)
          (O.metavar "DEFENDER[:LEVEL]")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Find good counters for a Pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      defenderVariants <-
        BattlerUtil.makeBattlerVariants gameMaster (defender options)

      -- attackers :: [[Pokemon]]
      attackers <- case attackerSource options of
        FromFiles filenames -> do
          let loadPokemon filename = do
                -- myPokemon is a [MyPokemon] with one MyPokemon for
                -- each entry in the file.  Each MyPokemon may have
                -- multiple possible IV sets.
                myPokemon <- join $ MyPokemon.load filename
                let myPokemon' =
                      map (doTweakLevel $ tweakLevel options) myPokemon
                -- makePokemon expands each MyPokemon into a [Pokemon] with
                -- one Pokemon per possible IV, so the result of the mapM
                -- (and of loadPokemon) is m [[Pokemon]].
                mapM (MakePokemon.makePokemon gameMaster) myPokemon'
          -- Load all the files and concat them into one [[Pokemon]].
          pokemonLists <- fmap concat $ mapM (loadPokemon . Just) filenames
--          [[A1, A2], [B1]]
--          [[A1/1, A2/1], [A1/2, A1/2], [B1/1], [B1/2]]
          let pokemonLists' = if showAllMovesets options
                then concat $ map (expandMoves gameMaster) pokemonLists
                else pokemonLists
          let maybeMaxCandy = Main.maybeMaxCandy options
              maybeMaxDust = Main.maybeMaxDust options
          if showPowerups options || Maybe.isJust maybeMaxCandy ||
              Maybe.isJust maybeMaxDust
            then return $
              -- Turn each [Pokemon] into multiple lists of powered-up
              -- Pokemon by turning each Pokemon into a list of
              -- powered up Pokmon and concatenating the lists.
              concat $ map (expandLevels gameMaster maybeMaxCandy maybeMaxDust)
                pokemonLists'
            else return pokemonLists'
        AllAttackers -> do
          mythicalMap <- join $ Mythical.load "mythical.yaml"
          let notMythical = not . Mythical.isMythical mythicalMap . PokemonBase.species
              notLegendary = not . Mythical.isLegendary mythicalMap . PokemonBase.species
              nonMythical = filter notMythical $ GameMaster.allPokemonBases gameMaster
              bases = if legendary options
                then nonMythical
                else filter notLegendary nonMythical
              ivs = IVs.tweakLevel (tweakLevel options) IVs.defaultIVs
              allAttackers = map
                (MakePokemon.makeWithAllMovesetsFromBase gameMaster ivs) bases
          -- map (:[]) $ concat
          -- turns [[attacker1a, attacker1b], [attacker2a, attacker2b]]
          -- into [[attacker1a], [attacker1b], [attacker2a], [attacker2b]]
          -- so a Result will be computed for each attack variant
          -- individuallly.
          if showAllMovesets options
            then return $ map (:[]) $ concat allAttackers
            else return $ allAttackers
        MovesetFor battlers ->
          let (errors, attackerLists) = Either.partitionEithers $
                map (makeWithAllMovesetsFromBattler gameMaster) battlers
          in case errors of
               [] -> return $ map (:[]) $ concat attackerLists
               _ -> Epic.fail $ List.intercalate "\n" $ map show errors

      let weatherBonus =
            GameMaster.getWeatherBonus gameMaster $ maybeWeather options
          friendBonus = Friend.damageBonus $ maybeFriend options
          makeBattle attacker defender = Battle.init attacker defender
            `Battle.setWeatherBonus` weatherBonus
            `Battle.setFriendBonus` friendBonus
            `Battle.setRaidGroup` (raidGroup options)
          doOneBattle attacker defender =
            Battle.doBattleOnly $ makeBattle attacker defender
          results =
            map (counter doOneBattle defenderVariants) attackers
          sortedByDps = List.reverse $ List.sortOn minDps results
          damagePerHp result = (fromIntegral $ minDamage result) /
            (fromIntegral $ Pokemon.hp $ pokemon result)
          sorted = case sortOutputBy options of
            ByDamage -> List.reverse $ List.sortOn minDamage results
            ByDps -> sortedByDps
            ByProduct -> List.reverse $
              List.sortOn (\result ->
                  minDps result * fromIntegral (minDamage result + 100))
                results
            ByDamagePerHp -> List.reverse $
              List.sortOn damagePerHp results
            ByTimeToFaint -> List.reverse $ List.sortOn timeToFaint results
            Weighted weight ->
              -- XXX Should this also subtract out dpsMin and damageMin?
              let damageMax = fromIntegral $ maximum $ map minDamage results
                  dpsMax = maximum $ map minDps results
                  weightedAverage result =
                    (fromIntegral $ minDamage result) / damageMax * weight +
                    (minDps result) / dpsMax * (1 - weight)
              in List.reverse $ List.sortOn weightedAverage results

          filtered = case dpsFilter options of
            Just n ->
              let dpsCutoff = minDps $ sortedByDps !! (length sortedByDps `div` n) 
                  dpsCutoffNames = Set.fromList $
                    map (Pokemon.pname . Main.pokemon) $
                    filter ((>= dpsCutoff) . minDps) results
              in filter ((`elem` dpsCutoffNames) . Pokemon.pname . Main.pokemon) sorted
            Nothing -> sorted
          nameFunc = case attackerSource options of
            FromFiles _ ->
              if showAllMovesets options
                then nameNameAndSpecies
                else nameNameAndSpeciesAndMoveset
            AllAttackers ->
              if showAllMovesets options
                then nameSpeciesAndLevelAndMoveset
                else nameSpeciesAndLevel
            MovesetFor _ -> nameSpeciesAndLevelAndMoveset

      case top options of
        Just n ->
          let topSpecies = take n $ List.nub $
                map (Pokemon.species . Main.pokemon) filtered
          in mapM_ putStrLn topSpecies
        Nothing -> forM_ filtered $ \ result -> do
          putStrLn $ showResult nameFunc result
          if showBreakpoints options
            then
              case pokemonVariants result of
                [attacker] ->
                  -- The defender moveset doesn't matter, so here we assume
                  -- all variants have the same level and IVs.
                  let defender = head defenderVariants
                      breakpoints = Breakpoint.getBreakpoints
                        gameMaster weatherBonus friendBonus
                        attacker defender
                  in case breakpoints of
                       (_:_:_) ->  -- Only show if there are two or more.
                         forM_ breakpoints $ \ (level, damage, dps) ->
                           putStrLn $ Printf.printf "  %-4s %d  %.1f"
                            (PokeUtil.levelToString level) damage dps
                       _ -> return ()
                _ -> putStrLn "  multiple attacker variants"
            else return ()
    )
    $ Exit.die

-- pokemonList represents a single pokemon with multiple possible IVs,
-- e.g., [P/iv1, P/iv2, P/iv3].  Each element is mapped to P/ivN for
-- each moveset so we end up with [[P/iv1/m1, [P/iv1/m2], [P/iv2/m1,
-- P/iv2/m2]].
--
expandMoves :: GameMaster -> [Pokemon] -> [[Pokemon]]
expandMoves gameMaster pokemonList =
  let typicalPokemon = head pokemonList
      base = Pokemon.base typicalPokemon
      existingQuickMove = Pokemon.quick typicalPokemon
      existingChargeMove = Pokemon.charge typicalPokemon
      allMovesets = [(quick, charge) |
        quick <- PokemonBase.quickMoves base,
        charge <- PokemonBase.chargeMoves base]
      isExistingMoveset quick charge =
        quick == existingQuickMove &&
        charge == existingChargeMove
      notLegacyMove = not . Move.isLegacy
      -- We only want to show the existing moves and any moves we can TM to.
      movesetsToShow = filter (\ (quick, charge) ->
          (notLegacyMove quick || quick == existingQuickMove) &&
          (notLegacyMove charge || charge == existingChargeMove))
        allMovesets
      setMovesAndName quick charge pokemon =
        let marker =
              if isExistingMoveset quick charge
                then "*-" :: String
                else "  "
            name = Printf.printf "%s%s [%s/%s]"
              marker
              (Pokemon.pname pokemon)
              (Move.name quick)
              (Move.name charge)
        in Pokemon.setName name $
             PokeUtil.setMoves gameMaster quick charge pokemon
  in map (\ (quick, charge) ->
       map (setMovesAndName quick charge) pokemonList)
       movesetsToShow

showResult :: (Pokemon -> String) -> Result -> String
showResult nameFunc result =
  Printf.printf "%4.1f %5d  %s"
    (minDps result) (minDamage result) (nameFunc $ pokemon result)

nameNameAndSpecies :: Pokemon -> String
nameNameAndSpecies pokemon =
  Pokemon.pname pokemon ++ " (" ++ Pokemon.species pokemon ++ ")"

nameNameAndSpeciesAndMoveset :: Pokemon -> String
nameNameAndSpeciesAndMoveset pokemon =
  Pokemon.pname pokemon ++ " (" ++ Pokemon.species pokemon ++ ", " ++
    movesetString pokemon ++ ")"

movesetString :: Pokemon -> String
movesetString pokemon =
  (Move.name $ Pokemon.quick pokemon) ++ "/" ++
    (Move.name $ Pokemon.charge pokemon)

nameSpeciesAndLevel :: Pokemon -> String
nameSpeciesAndLevel pokemon =
  let speciesAndLevel = Printf.printf "%s:%f"
        (Pokemon.species pokemon)
        (IVs.level $ Pokemon.ivs pokemon)
      speciesAndLevel' = Regex.subRegex (Regex.mkRegex "\\.0$") speciesAndLevel ""
  in speciesAndLevel'

nameSpeciesAndLevelAndMoveset :: Pokemon -> String
nameSpeciesAndLevelAndMoveset pokemon =
  Printf.printf "%-15s %-13s/ %-15s"
    (nameSpeciesAndLevel pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)

-- [A:iva:0, A:ivb:0] -> [[A:iva:0, A:ivb:0], [A<1>:iva:1, A<1>:ivb:1]]
--
expandLevels :: GameMaster -> Maybe Int -> Maybe Int -> [Pokemon] -> [[Pokemon]]
expandLevels gameMaster maybeMaxCandy maybeMaxDust pokemonList =
  let pokemonLevel = Pokemon.level $ head pokemonList
      powerupLevelsAndCosts =
        maybeFilter maybeMaxCandy
          (\ maxCandy (_, candy, _) -> candy <= maxCandy) $
        maybeFilter maybeMaxDust
          (\ maxDust (_, _, dust) -> dust <= maxDust) $
        Powerups.levelsAndCosts gameMaster Discounts.noDiscounts pokemonLevel
      powerupLevels = map (\ (lvl, _, _) -> lvl) powerupLevelsAndCosts
      addLevelToName pokemon = Pokemon.setName
        (Printf.printf "%s <%s>"
          (Pokemon.pname pokemon)
          (PokeUtil.levelToString $ Pokemon.level pokemon))
        pokemon
      setLevelAndName level pokemon =
        addLevelToName $ PokeUtil.setLevel gameMaster level pokemon
  in pokemonList :
       [map (setLevelAndName level) pokemonList | level <- powerupLevels]

maybeFilter :: Maybe a -> (a -> b -> Bool) -> [b] -> [b]
maybeFilter maybeA pred list =
  case maybeA of
    Nothing -> list
    Just a -> filter (pred a) list

doTweakLevel :: (Float -> Float) -> MyPokemon -> MyPokemon
doTweakLevel tweakLevel myPokemon =
  MyPokemon.setIVs myPokemon $ IVs.tweakLevel tweakLevel $
    MyPokemon.ivs myPokemon

counter :: (Pokemon -> Pokemon -> Battle) -> [Pokemon] -> [Pokemon] -> Result
counter doOneBattle defenderVariants attackerVariants =
  let battles = [doOneBattle attacker defender |
        attacker <- attackerVariants,
        defender <- defenderVariants]
      getMinValue fn = fn . List.minimumBy (Ord.comparing fn)
      minDamage = getMinValue Battle.damageInflicted battles
      minDps = getMinValue Battle.dps battles
      timeToFaint = getMinValue Battle.secondsElapsed battles
  in Result (head attackerVariants) attackerVariants minDps minDamage
       timeToFaint

allAttackers :: GameMaster -> IVs -> [[Pokemon]]
allAttackers gameMaster ivs =
  map (:[]) $ concat
    $ map (MakePokemon.makeWithAllMovesetsFromBase gameMaster ivs)
    $ GameMaster.allPokemonBases gameMaster

makeWithAllMovesetsFromBattler :: (Epic.MonadCatch m) => GameMaster -> Battler -> m [Pokemon]
makeWithAllMovesetsFromBattler gameMaster battler = do
  let species = BattlerUtil.species battler
  base <- GameMaster.getPokemonBase gameMaster species
  ivs <- case BattlerUtil.level battler of
    Normal ivs -> return ivs
    _ -> Epic.fail $ "Counter.makeSomeAttackers called for raid boss"
  return $ MakePokemon.makeWithAllMovesetsFromBase gameMaster ivs base
