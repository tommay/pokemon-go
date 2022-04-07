-- Generic has something to do with making Attacker an instance of Hashable.
{-# LANGUAGE DeriveGeneric #-} -- For deriving Hashable instance.
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Rarity (Rarity (..))
import qualified Util

import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)

import           Control.Applicative (optional, some, many)
import           Control.Monad (join)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

import qualified Debug as D

data OutputSpec = OutputSpec Int (Maybe FilePath)
  deriving (Show)

data Options = Options {
  level :: Float,
  attackersFile :: Maybe FilePath,
  outputs :: [OutputSpec]
  }

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLevel <*> optAttackers <*> optOutputs
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
      optOutputs =
        let deflt value [] = value
            deflt _ lst = lst
        in (deflt [OutputSpec 10 Nothing]) <$>
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
        maybeFilename <- optional $ do
          Atto.char ':'
          some $ Atto.anyChar
        Atto.endOfInput
        return $ OutputSpec n maybeFilename
  in case Atto.parseOnly attoParseOutputSpec (Text.pack string) of
    Left _ ->
      Left $ "`" ++ string ++ "' should look like N[:FILENAME]"
    Right outputSpec -> Right outputSpec

-- Since I use this for finding the best raid attackers, use decent
-- raid attacker IVs instead of IVs.defaultIvs.
--
defaultIvs = IVs.new 35 15 13 13

-- Attacker is name, quickName, chargeName.

data Attacker = Attacker String String String
  deriving (Show, Eq, Generic)

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
  defender :: PokemonBase,
  attackerResults :: [AttackerResult]
  } deriving (Show)

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load

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

      -- Smeargle's moves are handled strangely in GAME_MASTER.yaml and
      -- it currently ends up with an empty move list which causes head to
      -- fail.  For now, filter it out here.

      let defenderBases =
            filter ((/= "smeargle") . PokemonBase.species) allBases

          -- Add to each defender a list of all attackers.
          --
          defendersWithAttackers =
            map (getAttackerResults gameMaster attackers)
              defenderBases

      mapM_ (makeAndPrintOutputs defendersWithAttackers) $
-- works:
        [OutputSpec 1 (Just "/tmp/elites-1.out")]
-- But blows up if a second one is added:
--      mapM_ (makeAndPrintOutputs defendersWithAttackers) $
--        [OutputSpec 1 (Just "/tmp/elites-1.out")]
-- blows up:
--        [OutputSpec 1 (Just "/tmp/elites-1.out"),
--         OutputSpec 1 (Just "/tmp/elites-1.out")]
-- blows up:
--        outputs options
    )
    $ Exit.die

makeAndPrintOutputs :: [DefenderResult] -> OutputSpec -> IO ()
makeAndPrintOutputs defenderResults (OutputSpec n maybeFilename) =
  let defendersWithEliteAttackers =
        map (\ a -> a {
          attackerResults = take n $ attackerResults a
          }) defenderResults

      eliteLists = collectByAttacker defendersWithEliteAttackers

      outputStrings = map makeOutputString eliteLists

      outputString = concat $ map (++ "\n") outputStrings

      writeTheString = case maybeFilename of
        Nothing -> putStr
        Just filename -> writeFile filename

  in writeTheString outputString

makeOutputString :: (Attacker, [PokemonBase]) -> String
makeOutputString (attacker, victims) =
  (showAttacker attacker) ++ " => " ++
    (List.intercalate ", " $
      List.sort $ map PokemonBase.species victims)

showAttacker :: Attacker -> String
showAttacker (Attacker species fast charged) =
  Printf.printf "%s %s / %s" species fast charged

getAttackerResults :: GameMaster -> [Pokemon] -> PokemonBase -> DefenderResult
getAttackerResults gameMaster attackers defenderBase =
  let tier = case PokemonBase.rarity defenderBase of
        -- Battle legendary and mythic as tier 5 bosses, everything
        -- else as tier 3.
        Rarity.Legendary -> 5
        Rarity.Mythic -> 5
        _ -> 3
      defenderAllMoves = 
        BattlerUtil.makeRaidBossForTier gameMaster tier defenderBase
      attackerResults = reverse $ List.sortOn dps $
        map (getAttackerResult tier defenderAllMoves) attackers
  in DefenderResult {
       defender = defenderBase,
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

collectByAttacker :: [DefenderResult] -> [(Attacker, [PokemonBase])]
collectByAttacker defenderResults =
  HashMap.toList $ foldr (\ defenderResult attackerMap ->
      foldr (\ attackerResult attackerMap'->
          let defender' = defender defenderResult
              attacker' = attacker attackerResult
          in HashMap.insertWith (++) attacker' [defender'] attackerMap')
        attackerMap
        (attackerResults defenderResult))
    HashMap.empty
    defenderResults

makeAttacker :: Pokemon -> Attacker
makeAttacker pokemon =
  Attacker
    (Pokemon.species pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)
