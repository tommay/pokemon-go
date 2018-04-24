module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import qualified BattlerUtil
import           BattlerUtil (Battler, Level (Normal))
import qualified Epic
import qualified IVs
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Log
import qualified Logger
import qualified MakePokemon
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import           Pokemon (Pokemon)
import           Type (Type)

import qualified Debug

import           Control.Monad (join, mapM, forM_)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  filename :: String,
  defender :: Battler
}

data Result = Result {
  candy :: Int,
  stardust :: Int,
  dps :: Float,
  tdo :: Int
}

defaultIVs = IVs.new 30 11 11 11

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilename <*> optDefender
      optFilename = O.strOption
        (  O.long "file"
        <> O.short 'f'
        <> O.metavar "FILE"
        <> O.help "File to read my_pokemon from")
      optDefender = O.argument
        (BattlerUtil.optParseBattler defaultIVs) (O.metavar "DEFENDER[:LEVEL]")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc ("Create gnuplot data for plotting dps/tdo" ++
             " vs. powerup candy"))
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      let weatherBonus = GameMaster.getWeatherBonus gameMaster Nothing

      defenderVariants <-
        BattlerUtil.makeBattlerVariants gameMaster $ defender options

      -- myPokemon is a [MyPokemon] with one MyPokemon for
      -- each entry in the file.  Each MyPokemon may have
      -- multiple possible IV sets.
      myPokemon <- join $ MyPokemon.load $ Just $ filename options

      -- The plots of dps/tdo vs. candy/dust cost onlu make sense if
      -- all IV sets have the same level, so check that.

      forM_ myPokemon $ \ myPokemon ->
        case MyPokemon.ivs myPokemon of
          Nothing -> return () -- Ok for now, will blowout when making Pokemon.
          Just ivs ->
            let levels = map IVs.level ivs
            in if length levels == length (List.nub levels)
              then return ()
              else Epic.fail $
                MyPokemon.name myPokemon ++ " has multiple possible levels"

      forM_ myPokemon $ \ myPokemon -> do
        Printf.printf "\"%s\"\n" $ MyPokemon.name myPokemon
        results <-
          getResultsForAllPowerups gameMaster myPokemon defenderVariants
        forM_ results $ \ result -> do
          Printf.printf "%d %d %f %d\n"
            (candy result)
            (stardust result)
            (dps result)
            (tdo result)
        putStr "\n\n"
    )
    $ Exit.die

getResultsForAllPowerups ::
  Epic.MonadCatch m => GameMaster -> MyPokemon -> [Pokemon] -> m [Result]
getResultsForAllPowerups gameMaster myPokemon defenderVariants = do
  allPowerups <- allPowerups gameMaster myPokemon
  mapM (getResult gameMaster defenderVariants) allPowerups

getResult :: Epic.MonadCatch m =>
  GameMaster -> [Pokemon] -> (Int, Int, MyPokemon) -> m Result
getResult gameMaster defenderVariants (candy, stardust, myPokemon) = do
  attackerVariants <- MakePokemon.makePokemon gameMaster myPokemon
  let weatherBonus = GameMaster.getWeatherBonus gameMaster Nothing
      (dps, tdo) = getMinDpsTdo weatherBonus attackerVariants defenderVariants
  return Result {
    candy = candy,
    stardust = stardust,
    dps = dps,
    tdo = tdo
  }

getMinDpsTdo :: (Type -> Float) -> [Pokemon] -> [Pokemon] -> (Float, Int)
getMinDpsTdo weatherBonus attackerVariants defenderVariants =
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
  in (dps, tdo)

-- This set of functions to take a Pokemon and return a list of all
-- its powerups along with the required candy and stardust looks a
-- ittle overkill but it was done to build things up in small
-- potentially reusable steps instead of going it in one function.

allPowerups :: Epic.MonadCatch m => GameMaster -> MyPokemon -> m [(Int, Int, MyPokemon)]
allPowerups gameMaster myPokemon =
  iterateWhileJust (cumulativePowerup gameMaster) (0, 0, myPokemon)

-- This is like iterate but stops when the function returns Nothing.
-- Perhaps this could be done with something from Control.Monad.Loops,
-- but I don't understand a lot of that stuff.
--
iterateWhileJust :: Epic.MonadCatch m => (a -> m (Maybe a)) -> a -> m [a]
iterateWhileJust func a = do
  let iterateWhileJust' a = do
        case a of
          Nothing -> return []
          Just a -> do
            b <- func a
            as <- iterateWhileJust' b
            return $ a : as
  iterateWhileJust' $ Just a

-- Given a MyPokemon and the cumulative candy and stardust it took to
-- power it to its current level, power it up once more and return it
-- with the updated candy and dust costs.
--
cumulativePowerup :: Epic.MonadCatch m =>
  GameMaster -> (Int, Int, MyPokemon) -> m (Maybe (Int, Int, MyPokemon))
cumulativePowerup gameMaster (candy, stardust, myPokemon) = do
  powerup <- powerup gameMaster myPokemon
  return $ case powerup of
    Just (candy', stardust', myPokemon') ->
      Just (candy + candy', stardust + stardust', myPokemon')
    Nothing -> Nothing

-- Here we do thw actual powerup.  Returns Just (candy, stardust,
-- poweredUpMyPokemon) if the MyPokemon is not yet at max level, else
-- Nothing.
--
powerup :: Epic.MonadCatch m =>
  GameMaster -> MyPokemon -> m (Maybe (Int, Int, MyPokemon))
powerup gameMaster myPokemon = do
  level <- MyPokemon.level myPokemon
  return $ case GameMaster.nextLevel gameMaster level of
    Just (candy, dust, level) ->
      Just (candy, dust, MyPokemon.setLevel myPokemon level)
    Nothing -> Nothing
