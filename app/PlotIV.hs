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
import qualified Log
import qualified Logger
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
import qualified System.Exit as Exit
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  attacker :: String,
  defender :: Battler
}

defaultIVs = IVs.new 30 11 11 11

weatherBonus = const 1

getOptions :: IO Options
getOptions =
  let opts = Options <$> optAttacker <*> optDefender
      optAttacker = O.argument O.str
        (O.metavar "ATTACKER")
      optDefender = O.argument
        (BattlerUtil.optParseBattler defaultIVs) (O.metavar "DEFENDER[:LEVEL]")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc ("Create a gnplot script to plot dps and tdo for " ++
             "varying IV combonations")
        <> O.header "header - Plot dps and tdo vs. IVs")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

replacePercent :: String -> Int -> String
replacePercent string value =
  case Regex.matchRegexAll (Regex.mkRegex "%") string of
    Just (before, _, after, _) -> before ++ (show value) ++ after
    Nothing -> string

parseBattler :: (Epic.MonadCatch m) => String -> m Battler
parseBattler string =
  case BattlerUtil.parseBattler defaultIVs string of
    Right battler -> return battler
    Left errorString -> Epic.fail errorString

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      let weatherBonus = GameMaster.getWeatherBonus gameMaster Nothing

      let attackerString = Main.attacker options
          iv1Range =
            case Regex.matchRegex (Regex.mkRegex ":%") attackerString of
              Just _ -> [25..40]
              Nothing -> [0..15]
          ivs = [(iv1, iv2) | iv1 <- iv1Range, iv2 <- [0..15]]

      defenderVariants <-
        BattlerUtil.makeBattlerVariants gameMaster $ defender options

      forM_ ivs $ \ (iv1, iv2) -> do
        let a = replacePercent attackerString iv1
            b = replacePercent a iv2
        attacker <- parseBattler b
        attackerVariants <- BattlerUtil.makeBattlerVariants gameMaster attacker

        let battleLoggers =
              [Battle.runBattle $
                Battle.init weatherBonus attacker defender False |
                attacker <- attackerVariants, defender <- defenderVariants]
            -- battleResults :: [[Log Battle]], where each [Battle] is the
            -- move-by-move starting from the initial Battle state.
            battleResults = map Logger.execLogger battleLoggers
            blah = map (map Log.state) battleResults
            finalBattles = map last blah
            dps = minimum $ map Battle.dps finalBattles
            tdo = minimum $ map Battle.damageInflicted finalBattles

        Printf.printf "%d %d %f %d\n" iv1  iv2 dps tdo
    )
    $ Exit.die
