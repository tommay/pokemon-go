-- Generic has something to do with making Attacker an instance of Hashable.
{-# LANGUAGE DeriveGeneric #-} -- For deriving Hashable instance.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}  -- For deriving NFData.
{-# LANGUAGE TupleSections #-}

-- Simulates matchups between all attackers and defenders where attackers
-- are at the top of their evolution chain and defenders are tier 3
-- raid bosses or tier 5 if they're legendary or mythical.
-- Alternatively a file of attackers can be given and they will be
-- simulated against all defenders.
--
-- For each defender, all movesets are considered.
--
-- The attackers, including moveset, with the highest dps against the
-- defender are kept, and the high dps attacker/moveset combinations
-- are output with a list of the defenders they are high dps against.

-- A NOTE ON MEMORY USAGE: This should be able to run in limited
-- memory.  As each DefenderResult is calculated, it can be folded
-- into the HashMaps mapping each elite attacker to its victims and
-- can then be garbage collected along with associated thunks.  This
-- works fine as long as the list of (OutputSpec, EliteMap) passed to
-- (foldl' defenderResult) is hardcoded to a single element.  If it's
-- a list of arbitray length then the EliteMaps are only computed as
-- they're needed, so first the EliteMap for the first OutputSpec is
-- reduced and output and its thunks can be garbage collected.  But
-- the data and thunks for the subsequent OutputSpecs needs to
-- accumulate and stick around until it's needed when the data is
-- printed.  We'd really like everything to be reduced as the
-- DefenderResults are folded into the EliteMaps, whenever that is.
-- We don't easily have control over that.  So I use DeepSeq.force
-- before returning the result from foldDefenderResult adding to the
-- EliteMaps in the hope that it will cause all EliteMaps to be
-- resolved (so far) when the first one is required.

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))

import qualified Battle
import           Battle (Battle)
import qualified BattlerUtil
import qualified Epic
import qualified IVs
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import qualified MakePokemon
import qualified MyPokemon
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokemonClass (PokemonClass (..))
import qualified Util

import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)

import           Control.DeepSeq (NFData, force)

import           Control.Applicative (optional, some, many)
import           Control.Monad (join)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified System.Directory
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error

import qualified Text.Printf as Printf

import qualified Debug as D

type Defender = String

type EliteMap = HashMap Attacker [Defender]

data OutputSpec = OutputSpec {
  n :: Int,
  noRedundant :: Bool,
  maybeFilePath :: Maybe FilePath
  } deriving (Show, Generic, NFData)

data Options = Options {
  level :: Float,
  attackersFile :: Maybe FilePath,
  excludes :: [String],
  maybeDirectory :: Maybe String,
  outputs :: [OutputSpec]
  }

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLevel <*> optAttackers <*> optExcludes <*>
        optMaybeDirectory <*> optOutputs
      optLevel = O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.metavar "LEVEL"
        <> O.value (IVs.level defaultIvs)
        <> O.showDefault
        <> O.help "Set the level of the attackers")
      optAttackers = O.optional $ O.strOption
        (  O.long "attackers"
        <> O.short 'a'
        <> O.metavar "ATTACKERS-FILE"
        <> O.help "File with attacking pokemon, default all")
      optExcludes = (O.many . O.strOption)
        (  O.long "exclude"
        <> O.short 'x'
        <> O.metavar "POKEMON"
        <> O.help "Pokemon to exclude from elite consideration")
      optMaybeDirectory = O.optional $ O.strOption
        (  O.long "dir"
        <> O.short 'd'
        <> O.metavar "DIRECTORY"
        <> O.value ""
        <> O.help "Specify the output directory")
      optOutputs =
        let deflt value [] = value
            deflt _ lst = lst
            defaultOutputSpec = OutputSpec {
              n = 10, noRedundant = False, maybeFilePath = Nothing
              }
        in deflt [defaultOutputSpec] <$>
          (O.many . O.option optParseOutputSpec)
          (  O.long "output"
          <> O.short 'o'
          <> O.metavar "N:FILENAME"
          <> O.help
               "Output the top N pokemon by dps for each defender to the file")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc
          "Simulate matchups between all pokemon against tier 3 raid bosses")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

optParseOutputSpec :: O.ReadM OutputSpec
optParseOutputSpec = O.eitherReader parseOutputSpec

parseOutputSpec :: String -> Either String OutputSpec
parseOutputSpec string =
  let attoParseOutputSpec = do
        n <- Atto.decimal
        noRedundant <- Maybe.isJust <$> (optional $ Atto.char 'n')
        maybeFilePath <- optional $ do
          Atto.char ':'
          some $ Atto.anyChar
        Atto.endOfInput
        return $ OutputSpec {
          n = n,
          noRedundant = noRedundant,
          maybeFilePath = maybeFilePath
          }
  in case Atto.parseOnly attoParseOutputSpec (Text.pack string) of
    Left _ ->
      Left $ "`" ++ string ++ "' should look like N[n][:FILENAME]"
    Right outputSpec -> Right outputSpec

-- Since I use this for finding the best raid attackers, use decent
-- raid attacker IVs instead of IVs.defaultIvs.
--
defaultIvs = IVs.new 35 15 13 13

-- Attacker is name, quickName, chargeName.

data Attacker = Attacker String String String
  deriving (Show, Eq, Generic, NFData)

instance Hashable Attacker

-- Result of a particular attacker/moveset against all defender
-- movesets.  The pokemon is expected to score at least minDamage no
-- matter what the defender's moveset is.
--
-- The fields all use "!" to force strict evaluation so the computed battle
-- values are evaluated when the AttackerResult is created and the large trees
-- of thunks can be incrementally garbage collected, without running out of
-- memory.
--
data AttackerResult = AttackerResult {
  attacker  :: !Attacker,
  minDamage :: !Int,
  maxDamage :: !Int,
  dps       :: !Float
  } deriving (Show)

data DefenderResult = DefenderResult {
  defender :: Defender,
  attackerResults :: [AttackerResult]
  } deriving (Show)

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load

      let maybeDirectory = Main.maybeDirectory options
      case maybeDirectory of
        Nothing -> return ()
        Just directory -> ensureDirectoryExists directory

      let ivs = IVs.setLevel defaultIvs $ level options

          -- For now exclude megas.
          allBases = filter (not . PokemonBase.isMega) $
            GameMaster.allPokemonBases gameMaster

      attackers <- case attackersFile options of
        Just filename -> do
          myPokemon <- join $ MyPokemon.load $ Just filename
          mapM (fmap head . MakePokemon.makePokemon gameMaster) myPokemon
        Nothing -> do
          let attackerBases = filter (not . PokemonBase.hasEvolutions) allBases
          return $ concat $ map
            (MakePokemon.makeWithAllMovesetsFromBase gameMaster ivs)
            attackerBases
      attackers <- return $
        filter (not . (`elem` excludes options) . Pokemon.species) attackers

      -- Smeargle's moves are handled strangely in GAME_MASTER.yaml and
      -- it currently ends up with an empty move list which causes head to
      -- fail.  For now, filter it out here.

      let defenderBases =
            filter ((/= "smeargle") . PokemonBase.species) allBases

          -- Add to each defender a list of all attackers.
          --
          defenderResults =
            map (getAttackerResults gameMaster attackers) defenderBases

          eliteMaps = List.foldl' foldDefenderResult
            (map (, HashMap.empty) $ (case maybeDirectory of
              Nothing -> id
              Just directory -> map $ addDirectory directory) $
              outputs options) defenderResults

      mapM_ printEliteAtackers eliteMaps
    )
    $ Exit.die

-- Give a decent error message.  Without this, the system gives the
-- same message but prefixed with the executable name which is ugly
-- and confusing because I invoke it through a symlink with a
-- different name.
--
ensureDirectoryExists :: String -> IO ()
ensureDirectoryExists directory = do
  either <- System.IO.Error.tryIOError $
    System.Directory.createDirectoryIfMissing False directory
  case either of
    Left ioError -> Epic.fail $ show ioError
    Right _ -> return ()

addDirectory :: String -> OutputSpec -> OutputSpec
addDirectory directory outputSpec =
  outputSpec {
    maybeFilePath = ((directory ++ "/") ++) <$> maybeFilePath outputSpec
    }

foldDefenderResult :: [(OutputSpec, EliteMap)] -> DefenderResult ->
  [(OutputSpec, EliteMap)]
foldDefenderResult attackerMaps defenderResult =
  force $ map (foldDefenderResultIntoEliteMap defenderResult) attackerMaps

foldDefenderResultIntoEliteMap :: DefenderResult ->
  (OutputSpec, EliteMap) -> (OutputSpec, EliteMap)
foldDefenderResultIntoEliteMap defenderResult (outputSpec, attackerMap) =
  (outputSpec,
    List.foldl' (foldAttackerResult $ defender defenderResult) attackerMap $
      take (n outputSpec) $ attackerResults defenderResult)

foldAttackerResult :: Defender -> EliteMap -> AttackerResult ->
  EliteMap
foldAttackerResult defender attackerMap attackerResult =
  HashMap.insertWith (++) (attacker attackerResult) [defender] attackerMap

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen bool f a = if bool then f a else a

printEliteAtackers :: (OutputSpec, EliteMap) -> IO ()
printEliteAtackers (outputSpec, eliteMap) =
  let eliteMap' = applyWhen (noRedundant outputSpec) filterRedundant eliteMap
      outputString = unlines $ map makeOutputString $ HashMap.toList eliteMap'
      writeTheString = case maybeFilePath outputSpec of
        Nothing -> putStr
        Just filename -> writeFile filename
  in writeTheString outputString

filterRedundant :: EliteMap -> EliteMap
filterRedundant eliteMap  =
  HashMap.filter (not . isRedundant eliteMap) eliteMap

-- An attacker is outclassed if there is another attacker whose victim
-- list is a strict superset of the given attacker's victim list.  It
-- doesn't matter what a given attacker is outclassed by, just that it
-- is strictly outclassed.
--
isRedundant :: EliteMap -> [Defender] -> Bool
isRedundant eliteMap defenders =
  List.any (defenders `isProperSubset`) $ HashMap.elems eliteMap

isProperSubset :: Eq a => [a] -> [a] -> Bool
subset `isProperSubset` superset =
  length subset < length superset && List.all (`elem` superset) subset

makeOutputString :: (Attacker, [Defender]) -> String
makeOutputString (attacker, defenders) =
  (showAttacker attacker) ++ " => " ++
    (List.intercalate ", " $ List.sort defenders)

showAttacker :: Attacker -> String
showAttacker (Attacker species fast charged) =
  Printf.printf "%s %s / %s" species fast charged

getAttackerResults :: GameMaster -> [Pokemon] -> PokemonBase -> DefenderResult
getAttackerResults gameMaster attackers defenderBase =
  let tier = if PokemonBase.isMega defenderBase
         then
           case PokemonBase.pokemonClass defenderBase of
             PokemonClass.Legendary -> 6
             PokemonClass.Mythic -> 6
             _ -> 4
         else
           case PokemonBase.pokemonClass defenderBase of
             -- Battle legendary and mythic as tier 5 bosses, everything
             -- else as tier 3.
             PokemonClass.Legendary -> 5
             PokemonClass.Mythic -> 5
             PokemonClass.UltraBeast -> 5
             _ -> 3
      defenderAllMoves = 
        BattlerUtil.makeRaidBossForTier gameMaster tier defenderBase
      attackerResults = reverse $ List.sortOn dps $
        map (getAttackerResult tier defenderAllMoves) attackers
  in DefenderResult {
       defender = PokemonBase.species defenderBase,
       attackerResults = attackerResults
       }

getAttackerResult :: Int -> [Pokemon] -> Pokemon -> AttackerResult
getAttackerResult tier defenderAllMoves attacker =
  let battles =
        map (Battle.doBattleOnly . Battle.init attacker) defenderAllMoves
  in AttackerResult {
       attacker = makeAttacker attacker,
       minDamage = minimum $ map Battle.damageInflicted battles,
       maxDamage = maximum $ map Battle.damageInflicted battles,
       dps = minimum $ map Battle.dps battles
       }

-- Keep AttackerResults with damage >= 90% of the maximum damage.
-- This may keep only one AttackerResult if no other attacker even
-- comes close to the maximum.
--
keepTopDamageResults :: [AttackerResult] -> [AttackerResult]
keepTopDamageResults attackerResults =
  let damageCutOff =
        (List.maximum $ map minDamage attackerResults) * 9 `div` 10
  in filter ((>= damageCutOff) . minDamage) attackerResults

makeAttacker :: Pokemon -> Attacker
makeAttacker pokemon =
  Attacker
    (Pokemon.species pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)
