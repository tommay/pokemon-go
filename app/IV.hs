module Main where

import qualified Appraisal
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
      let (errors, myNewPokemon) =
            mapEither (updateIVs gameMaster new' ivFloor') myPokemon
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

mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither fn list =
  let result = map fn list
  in (Either.lefts result, Either.rights result)

updateIVs :: Epic.MonadCatch m => GameMaster -> Bool -> Int -> MyPokemon -> m MyPokemon
updateIVs gameMaster new ivFloor myPokemon = Epic.catch (
  do
    newIVs <- computeIVs gameMaster new ivFloor myPokemon
    return $ MyPokemon.setIVs myPokemon $ Just newIVs
  )
  $ \ex -> Epic.fail $
      "Problem with " ++ MyPokemon.name myPokemon ++ ": " ++ ex

computeIVs :: (Epic.MonadCatch m) => GameMaster -> Bool -> Int -> MyPokemon -> m [IVs]
computeIVs gameMaster new ivFloor myPokemon = do
  pokemonBase <- GameMaster.getPokemonBase gameMaster $ MyPokemon.species myPokemon
  possibleLevels <- GameMaster.getLevelsForStardust gameMaster
    $ MyPokemon.stardust myPokemon
  possibleIvs <- do
    appraisal <- Appraisal.new $ MyPokemon.appraisal myPokemon
    return $ Appraisal.possibleIvs appraisal
  let hasNoIvs = Maybe.isNothing . MyPokemon.ivs
  possibleIvs <- return $ if hasNoIvs myPokemon
    then filter (checkIvFloor ivFloor) possibleIvs
    else possibleIvs
  possibleIVs <- do
    let allIVs = [IVs.new level attack defense stamina |
          level <- possibleLevels,
          (attack, defense, stamina) <- possibleIvs]
        isWholeLevel s =
          let level = IVs.level s
          in fromIntegral (floor level) == level
        allIVs' = if new && hasNoIvs myPokemon
          then filter isWholeLevel allIVs
          else allIVs
        ivsMatchMyPokemon ivs =
          MyPokemon.hp myPokemon ==
            Calc.hp gameMaster pokemonBase ivs &&
          let calcCp = Calc.cp gameMaster pokemonBase
          in MyPokemon.cp myPokemon == calcCp ivs &&
               case MyPokemon.powerup myPokemon of
                 Nothing -> True
                 Just powerup -> powerup == calcCp (IVs.tweakLevel (+ 0.5) ivs)
        ivsThatMatchMyPokemon = filter ivsMatchMyPokemon allIVs'
    case ivsThatMatchMyPokemon of
      [] -> Epic.fail "No possible ivs"
      matchingIVs -> return $ do
        case MyPokemon.ivs myPokemon of
          Nothing -> matchingIVs
          Just currentIVs ->
            filter (\matching ->
              any (\current ->
                let ivs = sequence [IVs.attack, IVs.defense, IVs.stamina]
                in ivs matching == ivs current)
              currentIVs)
            matchingIVs
  case possibleIVs of
    [] -> Epic.fail "No possible remaining ivs"
    _ -> return possibleIVs

checkIvFloor :: Int -> (Int, Int, Int) -> Bool
checkIvFloor ivFloor (attack, defense, stamina) =
  attack >= ivFloor && defense >= ivFloor && stamina >= ivFloor

updateLevel :: (Float -> Float) -> MyPokemon -> MyPokemon
updateLevel tweakLevel myPokemon =
  MyPokemon.setIVs myPokemon $
    map (IVs.tweakLevel tweakLevel) <$> MyPokemon.ivs myPokemon
