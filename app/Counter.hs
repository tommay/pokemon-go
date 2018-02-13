{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import qualified BattlerUtil
import           BattlerUtil (Battler, Level (Normal))
import qualified Breakpoint
import qualified Epic
import qualified TweakLevel
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
import qualified Type
import           Type (Type)
import qualified Util
import qualified Weather
import           Weather (Weather (..))

import qualified Debug

import           Control.Monad (join, forM, forM_)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified System.IO as I
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  maybeWeather :: Maybe Weather,
  sortOutputBy :: SortOutputBy,
  dpsFilter :: Maybe Int,
  top      :: Maybe Int,
  tweakLevel :: Float -> Float,
  legendary :: Bool,
  attackerSource :: AttackerSource,
  showBreakpoints :: Bool,
  defender :: Battler
}

data SortOutputBy =
  ByDamage | ByDps | ByProduct | ByDamagePerHp | Weighted Float
  deriving (Show)

data AttackerSource =
    FromFiles [FilePath]
  | AllAttackers
  | MovesetFor [Battler]
  deriving (Show)

data Result = Result {
  pokemon   :: Pokemon,
  minDps    :: Float,
  minDamage :: Int
} deriving (Show)

defaultFilename = "my_pokemon.yaml"
defaultIVs = IVs.new 30 11 11 11
defaultAttackerLevel = IVs.level defaultIVs

getOptions :: IO Options
getOptions =
  let opts = Options <$> optWeather <*> optSortOutputBy <*> optDpsFilter <*>
        optTop <*>
        optTweakLevel <*> optLegendary <*> optAttackerSource <*>
        optShowBreakpoints <*> optDefender
      optWeather = O.optional Weather.optWeather
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
            optWeighted = Weighted <$> O.option O.auto
              (  O.long "weighted"
              <> O.short 'w'
              <> O.metavar "WEIGHT"
              <> O.help ("Sort by weighted mix of dps and damage, " ++
                   "where weight varies from 0.0 for pure dps to " ++
                   "1.0 for pure damage"))
        in optGlass <|> optProduct <|> optDamagePerHp
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
                  (BattlerUtil.parseBattler defaultIVs))
              (  O.long "moveset"
              <> O.short 'm'
              <> O.metavar "ATTACKER[:LEVEL]"
              <> O.help "Rate the movesets for ATTACKER against DEFENDER")
        in optFilenames <|> optMovesetFor <|> optAll
             <|> (pure $ FromFiles [defaultFilename])
      optShowBreakpoints = O.switch
        (  O.long "breakpoints"
        <> O.short 'k'
        <> O.help "Show attacker breakpoints")
      optDefender = O.argument
        (BattlerUtil.parseBattler defaultIVs)
          (O.metavar "DEFENDER[:LEVEL]")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Find good counters for a Pokemon."
        <> O.header "header - Find good counters for a Pokemon.")
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
                myPokemon <- join $ MyPokemon.load filename
                let myPokemon' =
                      map (doTweakLevel $ tweakLevel options) myPokemon
                mapM (MakePokemon.makePokemon gameMaster) myPokemon'
          fmap concat $ mapM (loadPokemon . Just) filenames
        AllAttackers -> do
          mythicalMap <- join $ Mythical.load "mythical.yaml"
          let notMythical = not . Mythical.isMythical mythicalMap . PokemonBase.species
              notLegendary = not . Mythical.isLegendary mythicalMap . PokemonBase.species
              nonMythical = filter notMythical $ GameMaster.allPokemonBases gameMaster
              bases = if legendary options
                then nonMythical
                else filter notLegendary nonMythical
              ivs = IVs.tweakLevel (tweakLevel options) defaultIVs
          return $ map
            (MakePokemon.makeWithAllMovesetsFromBase gameMaster ivs) bases
        MovesetFor battlers ->
          let attackerSpecies = map BattlerUtil.species battlers
          in case filter (not . GameMaster.isSpecies gameMaster) attackerSpecies of
               [] -> mapM (makeWithAllMovesetsFromBattler gameMaster) battlers
               noSuchSpecies -> Epic.fail $ "No such species: " ++ (List.intercalate ", " noSuchSpecies)

      let weatherBonus = case maybeWeather options of
            Just weather -> GameMaster.getWeatherBonus gameMaster weather
            Nothing -> const 1
          results = map (counter weatherBonus defenderVariants) attackers
          sortedByDps = List.reverse $ Util.sortWith minDps results
          damagePerHp result = (fromIntegral $ minDamage result) /
            (fromIntegral $ Pokemon.hp $ pokemon result)
          sorted = case sortOutputBy options of
            ByDamage -> List.reverse $ Util.sortWith minDamage results
            ByDps -> sortedByDps
            ByProduct -> List.reverse $
              List.sortBy (Util.compareWith $ \result ->
                  minDps result * fromIntegral (minDamage result + 100))
                results
            ByDamagePerHp -> List.reverse $
              Util.sortWith damagePerHp results
            Weighted weight ->
              -- XXX Should this also subtract out dpsMin and damageMin?
              let damageMax = fromIntegral $ maximum $ map minDamage results
                  dpsMax = maximum $ map minDps results
                  weightedAverage result =
                    (fromIntegral $ minDamage result) / damageMax * weight +
                    (minDps result) / dpsMax * (1 - weight)
              in List.reverse $ Util.sortWith weightedAverage results

          filtered = case dpsFilter options of
            Just n ->
              let dpsCutoff = minDps $ sortedByDps !! (length sortedByDps `div` n) 
                  dpsCutoffNames = Set.fromList $
                    map (Pokemon.pname . Main.pokemon) $
                    filter ((>= dpsCutoff) . minDps) results
              in filter ((`elem` dpsCutoffNames) . Pokemon.pname . Main.pokemon) sorted
            Nothing -> sorted
          nameFunc = case attackerSource options of
            FromFiles _ -> nameName
            AllAttackers -> nameSpeciesAndLevelAndMoveset
            MovesetFor _ -> nameSpeciesAndLevelAndMoveset

      case top options of
        Just n ->
          let topSpecies = take n $ List.nub $
                map (Pokemon.species . Main.pokemon) filtered
          in mapM_ putStrLn topSpecies
        Nothing -> forM_ filtered $ \ result -> do
          putStrLn $ showResult nameFunc result
          if showBreakpoints options
            then do
              let defender = head defenderVariants
                  breakpoints = Breakpoint.getBreakpoints
                    gameMaster weatherBonus
                    (pokemon result) defender
              forM_ breakpoints $ \ (level, damage) ->
                putStrLn $ Printf.printf
                  "  %-4s %d" (PokeUtil.levelToString level) damage
            else return ()
    )
    $ I.hPutStrLn I.stderr

showResult :: (Pokemon -> String) -> Result -> String
showResult nameFunc result =
  Printf.printf "%4.1f %5d  %s"
    (minDps result) (minDamage result) (nameFunc $ pokemon result)

nameName :: Pokemon -> String
nameName pokemon =
  Pokemon.pname pokemon

nameSpeciesAndLevelAndMoveset :: Pokemon -> String
nameSpeciesAndLevelAndMoveset pokemon =
  let speciesAndLevel = Printf.printf "%s:%f"
        (Pokemon.species pokemon)
        (IVs.level $ Pokemon.ivs pokemon)
      speciesAndLevel' = Regex.subRegex (Regex.mkRegex "\\.0$") speciesAndLevel ""
      format = Printf.printf "%-15s %-13s/ %-15s"
  in format speciesAndLevel'
       (Move.name $ Pokemon.quick pokemon)
       (Move.name $ Pokemon.charge pokemon)

doTweakLevel :: (Float -> Float) -> MyPokemon -> MyPokemon
doTweakLevel tweakLevel myPokemon =
  let ivs = MyPokemon.ivs myPokemon
      ivs' = (fmap $ map $ IVs.tweakLevel tweakLevel) ivs
  in MyPokemon.setIVs myPokemon ivs'

counter :: (Type -> Float) -> [Pokemon] -> [Pokemon] -> Result
counter weatherBonus defenderVariants attackerVariants =
  let battles = [Battle.runBattleOnly $
        Battle.init weatherBonus attacker defender |
        attacker <- attackerVariants,
        defender <- defenderVariants]
      getMinValue fn = fn . List.minimumBy (Util.compareWith fn)
      minDamage = getMinValue Battle.damageInflicted battles
      minDps = getMinValue Battle.dps battles
  in Result (head attackerVariants) minDps minDamage

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
