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
import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Text.Printf as Printf

import qualified Debug as D

type Defender = String

type EliteMap = HashMap Attacker [Defender]

data OutputSpec = OutputSpec {
  n :: Int,
  noRedundant :: Bool,
  includeMegaAttackers :: Bool
  } deriving (Show, Generic, NFData)

defaultOutputSpecs = [OutputSpec {
  n = 10,
  noRedundant = False,
  includeMegaAttackers = True
  }]

toString :: OutputSpec -> String
toString outputSpec =
  show (n outputSpec) ++
    if noRedundant outputSpec then "n" else "" ++
    if includeMegaAttackers outputSpec then "m" else ""

data Options = Options {
  level :: Float,
  attackersFile :: Maybe FilePath,
  excludes :: [String],
  -- An optionsl comma-separated list.
  maybeOutputSpecStrings :: Maybe String,
  -- Nothing to write to stdout.
  maybeFileTemplate :: Maybe String
  }

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLevel <*> optAttackers <*> optExcludes <*>
        optMaybeOutputSpecStrings <*> optMaybeFileTemplate
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
      optMaybeOutputSpecStrings =
        (O.optional . O.strOption) $
        (  O.long "output"
        <> O.short 'o'
        <> O.metavar "N[mn],..."
        <> O.help
             "Output the top N pokemon by dps for each defender")
      optMaybeFileTemplate =
        (O.optional . O.strOption )
        (  O.long "file"
        <> O.short 'f'
        <> O.metavar "FILETEMPLATE"
        <> O.help "Output filename, % will be replaced by each outputspec")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc
          "Simulate matchups between all pokemon against tier 3 raid bosses")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

parseOutputSpec :: String -> Either String OutputSpec
parseOutputSpec string =
  let attoParseOutputSpec = do
        n <- Atto.decimal
        flags <- Atto.many' $ Atto.satisfy $ Atto.inClass "mn"
        let noRedundant = 'n' `elem` flags
            includeMegaAttackers = 'm' `elem` flags
        Atto.endOfInput
        return $ OutputSpec {
          n = n,
          noRedundant = noRedundant,
          includeMegaAttackers = includeMegaAttackers
          }
  in case Atto.parseOnly attoParseOutputSpec (Text.pack string) of
    -- The error message isn't useful, just return the unparseable
    -- String to indicate failure.
    Left _ -> Left string
    Right outputSpec -> Right outputSpec

-- Since I use this for finding the best raid attackers, use decent
-- raid attacker IVs instead of IVs.defaultIvs.
--
defaultIvs = IVs.new 35 15 13 13

-- Attacker is name, quickName, chargeName, isMega.
--
data Attacker = Attacker String String String Bool
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

      outputSpecs <- case maybeOutputSpecStrings options of
        Nothing -> return defaultOutputSpecs
        Just outputSpecStrings ->
          let strings = Split.splitOn "," $ outputSpecStrings
          in case parseOutputSpecs strings of
               Left error -> Epic.fail error
               Right outputSpecs -> return outputSpecs

      let ivs = IVs.setLevel defaultIvs $ level options
          allBases = GameMaster.allPokemonBases gameMaster

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
            (map (, HashMap.empty) outputSpecs)
            defenderResults

      mapM_ (printEliteAtackers $ maybeFileTemplate options) eliteMaps
    )
    $ Exit.die

parseOutputSpecs :: [String] -> Either String [OutputSpec]
parseOutputSpecs strings =
  let (malformedStrings, outputSpecs) = Either.partitionEithers $
        map parseOutputSpec strings
  in case malformedStrings of
       [] -> Right outputSpecs
       _ -> Left $
         pluralize (List.length malformedStrings) "OutputSpec" ++ " " ++
         (Util.commaSeparated "and" malformedStrings) ++
         " should look like N[nm]."

pluralize :: Int -> String -> String
pluralize n =
  applyWhen (n > 1) (++ "s")

foldDefenderResult :: [(OutputSpec, EliteMap)] -> DefenderResult ->
  [(OutputSpec, EliteMap)]
foldDefenderResult eliteMaps defenderResult =
  force $ map (foldDefenderResultIntoEliteMap defenderResult) eliteMaps

foldDefenderResultIntoEliteMap :: DefenderResult ->
  (OutputSpec, EliteMap) -> (OutputSpec, EliteMap)
foldDefenderResultIntoEliteMap defenderResult (outputSpec, eliteMap) =
  (outputSpec,
    List.foldl' (foldAttackerResult $ defender defenderResult) eliteMap $
      take (n outputSpec) $
      applyWhen (not $ includeMegaAttackers $ outputSpec)
        (filter $ not . isMegaAttacker . attacker) $
      attackerResults defenderResult)

foldAttackerResult :: Defender -> EliteMap -> AttackerResult ->
  EliteMap
foldAttackerResult defender eliteMap attackerResult =
  HashMap.insertWith (++) (attacker attackerResult) [defender] eliteMap

isMegaAttacker :: Attacker -> Bool
isMegaAttacker (Attacker _ _ _ isMega) = isMega

isMega :: String -> Bool
isMega species = "mega_" `List.isPrefixOf` species

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen bool f a = if bool then f a else a

printEliteAtackers :: Maybe String -> (OutputSpec, EliteMap) -> IO ()
printEliteAtackers maybeFileTemplate (outputSpec, eliteMap) =
  let eliteMap' = applyWhen (noRedundant outputSpec) filterRedundant $ eliteMap
      outputString = unlines $ map makeOutputString $ HashMap.toList eliteMap'
      writeTheString = case maybeFileTemplate of
        Nothing -> putStr
        Just fileTemplate ->
          let filename = replace "%" (toString outputSpec) fileTemplate
          in writeFile filename
  in writeTheString outputString

replace :: String -> String -> String -> String
replace from to = Text.unpack .
  Text.replace (Text.pack from) (Text.pack to) .
  Text.pack

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
    (List.intercalate ", " $ sortDefenders defenders)

sortDefenders :: [Defender] -> [Defender]
sortDefenders defenders =
  let (mega, regular) = List.partition isMega defenders
  in List.sort regular ++ List.sort mega

showAttacker :: Attacker -> String
showAttacker (Attacker species fast charged _) =
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
    (PokemonBase.isMega $ Pokemon.base $ pokemon)
