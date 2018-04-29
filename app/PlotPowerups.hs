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
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil
import           Type (Type)
import qualified Util

import qualified Debug

import           Control.Monad (join, mapM, forM_)
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

data Options = Options {
  attackerSource :: AttackerSource,
  maybeEvolution :: Maybe String,
  defender :: Battler
}

data AttackerSource =
    FromFile FilePath
  | MovesetsFor Battler
  deriving (Show)

data Result = Result {
  candy :: Int,
  stardust :: Int,
  dps :: Float,
  tdo :: Int
}

defaultIVs = IVs.new 30 11 11 11

getOptions :: IO Options
getOptions =
  let opts = Options <$> optAttackerSource <*> optEvolution <*> optDefender
      optAttackerSource =
        let optFilename = FromFile <$>
              O.strOption
              (  O.long "file"
              <> O.short 'f'
              <> O.metavar "FILE"
              <> O.help "File to read my_pokemon from")
            optMovesetsFor = MovesetsFor <$>
              (O.option $ BattlerUtil.optParseBattler defaultIVs)
              (  O.long "movesets"
              <> O.short 'm'
              <> O.metavar "ATTACKER[:LEVEL]"
              <> O.help "Plot the movesets for ATTACKER against DEFENDER")
        in optFilename <|> optMovesetsFor
      optEvolution = O.optional $ O.strOption
        (  O.long "evolution"
        <> O.short 'e'
        <> O.metavar "EVOLUTION"
        <> O.help "Final evolution desired")
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

      myPokemon <- case attackerSource options of
        FromFile filename ->
          -- myPokemon is a [MyPokemon] with one MyPokemon for
          -- each entry in the file.  Each MyPokemon may have
          -- multiple possible IV sets.
          join $ MyPokemon.load $ Just filename
        MovesetsFor _ ->
          error "Can't do -m yet"

      -- The plots of dps/tdo vs. candy/dust cost only make sense if
      -- all IV sets have the same level, so split each MyPokemon into
      -- multiple pokemon with the same level in their IVs.

      myPokemon <- do return $ concat $ map splitByLevel myPokemon

      -- Evolve all pokemon to their highest level and remember how
      -- much candy it took.

      myPokemon <-
        mapM (evolveFully gameMaster $ maybeEvolution options) myPokemon

      forM_ myPokemon $ \ (myPokemon, candy) -> do
        Printf.printf "\"%s\"\n" $ MyPokemon.name myPokemon
        results <-
          getResultsForAllPowerups gameMaster candy myPokemon defenderVariants
        forM_ results $ \ result -> do
          Printf.printf "%d %d %f %d\n"
            (Main.candy result)
            (Main.stardust result)
            (Main.dps result)
            (Main.tdo result)
        putStrLn "e"
    )
    $ Exit.die

splitByLevel :: MyPokemon -> [MyPokemon]
splitByLevel myPokemon =
  case MyPokemon.ivs myPokemon of
    Nothing -> [myPokemon]
    Just ivs ->
      let groups = Util.groupBy IVs.level ivs
      in if HashMap.size groups == 1
           then [myPokemon]
           else
             map (\ (level, ivs) ->
               let myPokemon' = MyPokemon.setIVs myPokemon $ Just ivs
                   nameAndLevel = Printf.printf "%s (%s)"
                     (MyPokemon.name myPokemon)
                     (PokeUtil.levelToString level)
               in MyPokemon.setName myPokemon' nameAndLevel)
             $ List.sortBy (\(a,_) (b,_) -> compare a b)
               $ HashMap.toList groups

evolveFully :: Epic.MonadCatch m =>
  GameMaster -> Maybe String -> MyPokemon -> m (MyPokemon, Int)
evolveFully gameMaster maybeTarget myPokemon = do
  let species = MyPokemon.species myPokemon
  chains <- evolutionChains gameMaster (species, 0)
  chain <- case maybeTarget of
    Just target ->
      case filter ((== map Char.toLower target)
          . map Char.toLower . fst . List.last) chains of
        [] -> Epic.fail $ species ++ " does not evolve to " ++ target
        [chain] -> return chain
    Nothing ->
      case chains of
        [chain] -> return chain
        _ -> Epic.fail $ species ++ " has multiple possible evolutions"
  let (evolvedSpecies, candy) = List.last chain
  return $ (MyPokemon.setSpecies myPokemon evolvedSpecies, candy)

evolutionChains ::
  Epic.MonadCatch m => GameMaster -> (String, Int) -> m [[(String, Int)]]
evolutionChains gameMaster (species, candy) = do
  base <- GameMaster.getPokemonBase gameMaster species
  case PokemonBase.evolutions base of
    [] -> return [[(species, candy)]]
    evolutions -> do
      concat <$> (mapM (\ (evolution, candy') -> do
          rest <- evolutionChains gameMaster (evolution, candy + candy')
          return $ map ((species, candy):) rest))
        evolutions

getResultsForAllPowerups :: Epic.MonadCatch m =>
  GameMaster -> Int -> MyPokemon -> [Pokemon] -> m [Result]
getResultsForAllPowerups gameMaster candy myPokemon defenderVariants = do
  allPowerups <- allPowerups gameMaster candy myPokemon
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

allPowerups :: Epic.MonadCatch m =>
  GameMaster -> Int -> MyPokemon -> m [(Int, Int, MyPokemon)]
allPowerups gameMaster candy myPokemon =
  iterateWhileJust (cumulativePowerup gameMaster) (candy, 0, myPokemon)

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
