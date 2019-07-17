module Main where

import qualified Calc
import qualified Epic
import qualified TweakLevel
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified PokeUtil

import qualified Options.Applicative as O
import           Options.Applicative ((<**>), (<|>))
import           Control.Monad (join)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Semigroup ((<>))

import qualified Data.ByteString as B
import qualified Data.Yaml.Builder as Builder
import qualified System.Exit as Exit

data Options = Options {
  new       :: Bool,
  ivFloor   :: Int,
  stats     :: Bool,
  tweakLevel:: Float -> Float,
  maybeFilename :: Maybe String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optNew <*> optIvFloor <*> optStats <*> optGetLevel <*> optFilename
      optNew = O.switch
        (  O.long "new"
        <> O.short '0'
        <> O.help "Assume pokemon without ivs are newly caught or hatched")
      optIvFloor =
            O.flag' 1 (O.long "good" <> O.help "traded with good friend")
        <|> O.flag' 2 (O.long "great" <> O.help "traded with great friend")
        <|> O.flag' 3 (O.long "ultra" <> O.help "traded with ultra friend")
        <|> O.flag' 5 (O.long "best" <> O.help "traded with best friend")
        <|> O.flag' 10 (O.long "high" <>
              O.help "hatched, raid boss, research reward, or lucky")
        <|> pure 0
      optStats = O.switch
        (  O.long "stats"
        <> O.short 's'
        <> O.help "Include the complate base+iv * level stats")
      optGetLevel = TweakLevel.optTweakLevel 
      optFilename = O.optional $ O.argument O.str (O.metavar "FILENAME")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Calculate IVs for pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main = Epic.catch (
  do
    options <- getOptions

    gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

    myPokemon <- join $ MyPokemon.load $ maybeFilename options

    let new' = new options
        ivFloor' = ivFloor options
        tweakLevel' = tweakLevel options
    myNewPokemon <- do
      let (errors, myNewPokemon) = Either.partitionEithers $
            map (updateIVs gameMaster new' ivFloor') myPokemon
      case errors of
        [] -> do
          myNewPokemon <- return $ map (updateLevel tweakLevel') myNewPokemon
          if stats options
            then mapM (PokeUtil.addStats gameMaster) myNewPokemon
            else return myNewPokemon
        _ -> Epic.fail $ List.intercalate "\n" $ map show errors
          
    B.putStr $ Builder.toByteString myNewPokemon
  )
  $ Exit.die

updateIVs :: Epic.MonadCatch m => GameMaster -> Bool -> Int -> MyPokemon -> m MyPokemon
updateIVs gameMaster new ivFloor myPokemon = Epic.catch (
  do
    newIVs <- computeIVs gameMaster new ivFloor myPokemon
    return $ MyPokemon.setIVs myPokemon newIVs
  )
  $ \ex -> Epic.fail $
      "Problem with " ++ MyPokemon.name myPokemon ++ ": " ++ ex

computeIVs :: Epic.MonadCatch m => GameMaster -> Bool -> Int -> MyPokemon -> m IVs
computeIVs gameMaster new ivFloor myPokemon = do
  _ <- checkIvs gameMaster myPokemon
  return $ MyPokemon.ivs myPokemon

checkIvs :: (Epic.MonadCatch m) => GameMaster -> MyPokemon -> m ()
checkIvs gameMaster myPokemon = do
  pokemonBase <- GameMaster.getPokemonBase gameMaster
    $ MyPokemon.species myPokemon
  let ivs = MyPokemon.ivs myPokemon
  possibleLevels <- GameMaster.getLevelsForStardust gameMaster
    $ MyPokemon.stardust myPokemon
  case IVs.level ivs `elem` possibleLevels of
    False -> Epic.fail "Stardust and level are not consistent"
    True ->
      case MyPokemon.cp myPokemon == Calc.cp gameMaster pokemonBase ivs of
        False -> Epic.fail "IVs are not consistent with cp"
        True ->
          case MyPokemon.hp myPokemon == Calc.hp gameMaster pokemonBase ivs of
            False -> Epic.fail "IVs are not consistent with hp"
            True -> return ()

updateLevel :: (Float -> Float) -> MyPokemon -> MyPokemon
updateLevel tweakLevel myPokemon =
  MyPokemon.setIVs myPokemon $
    IVs.tweakLevel tweakLevel $
    MyPokemon.ivs myPokemon
