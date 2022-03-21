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

import           Control.Monad (join)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

import qualified Debug as D

data Options = Options {
  level :: Float,
  attackersFile :: Maybe FilePath,
  topNByDps :: Int
  }

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLevel <*> optAttackers <*> optN
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
      optN = O.option O.auto
        (  O.long "top"
        <> O.short 'n'
        <> O.metavar "N"
        <> O.value 10
        <> O.help "Select the top N pokemon by dps for each defender")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc
          "Simulate matchups between all pokemon against tier 3 raid bosses")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

-- Since I use this for fining the best raid attackers, use decent
-- raid attacker IVs instead of IVs.defaultIvs.
--
defaultIvs = IVs.new 35 15 13 13

-- Attacker is name, quickName, chargeName.

data Attacker = Attacker String String String
  deriving (Eq, Generic)

instance Hashable Attacker

-- Result of a particular attacker/moveset against all defender
-- movesets.  The pokemon is expected to score at least minDamage no
-- matter what the defender's moveset is.
--
-- The fields all use "!" to force strict evaluation so the computed battle
-- values are evaluated whtn the AttackerResult is created and the large trees
-- of thunks can be incrementally garbage collected, without running out of
-- memory.
--
data AttackerResult = AttackerResult {
  attacker  :: !Attacker,
  minDamage :: !Int,
  maxDamage :: !Int,
  dps       :: !Float
  }

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

          -- Add to each defender a list of all attackers with their
          -- worst-caseAttackerResult vs. the defender.
          --
          defendersWithBestAttackerResults =
            Util.augment (keepHighDpsResults (topNByDps options) .
              getAttackerResults gameMaster attackers)
              defenderBases

          byAttacker = collectByAttacker defendersWithBestAttackerResults

      mapM_ putStrLn $ map (\ (attacker, victims) ->
        (showAttacker attacker) ++ " => " ++
          (List.intercalate ", " $
            List.sort $ map PokemonBase.species victims))
        byAttacker
    )
    $ Exit.die

showAttacker :: Attacker -> String
showAttacker (Attacker species fast charged) =
  Printf.printf "%s %s / %s" species fast charged

getAttackerResults :: GameMaster -> [Pokemon] -> PokemonBase ->
  [AttackerResult]
getAttackerResults gameMaster attackers defenderBase =
  let tier = case PokemonBase.rarity defenderBase of
        -- Battle legendary and mythic as tier 5 bosses, everything
        -- else as tier 3.
        Rarity.Legendary -> 5
        Rarity.Mythic -> 5
        _ -> 3
      defenderAllMoves = 
        BattlerUtil.makeRaidBossForTier gameMaster tier defenderBase
  in map (getAttackerResult tier defenderAllMoves) attackers

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

-- Keep the top N AttackerResults by dps.
--
keepHighDpsResults :: Int -> [AttackerResult] -> [AttackerResult]
keepHighDpsResults n attackerResults =
  let sortedByDps = reverse $ List.sortOn dps attackerResults
  in take n sortedByDps

-- Keep AttackerResults with damage >= 90% of the maximum damage.
-- This may keep only one AttackerResult if no other attacker even
-- comes close to the maximum.
--
keepTopDamageResults :: [AttackerResult] -> [AttackerResult]
keepTopDamageResults attackerResults =
  let damageCutOff =
        (List.maximum $ map minDamage attackerResults) * 9 `div` 10
  in filter ((>= damageCutOff) . minDamage) attackerResults

collectByAttacker :: [(PokemonBase, [AttackerResult])] ->
  [(Attacker, [PokemonBase])]
collectByAttacker defendersWithAttackerResults =
  HashMap.toList $ foldr (\ (defender, attackerResults) attackerMap ->
      foldr (\ attackerResult attackerMap'->
          let attacker' = attacker attackerResult
          in HashMap.insertWith (++) attacker' [defender] attackerMap')
        attackerMap
        attackerResults)
    HashMap.empty
    defendersWithAttackerResults

makeAttacker :: Pokemon -> Attacker
makeAttacker pokemon =
  Attacker
    (Pokemon.species pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)
