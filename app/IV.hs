module Main where

import qualified Appraisal
import qualified Calc
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Stats
import           Stats (Stats)

import qualified Options.Applicative as O
import           Options.Applicative ((<**>))
import qualified Data.Maybe as Maybe
import           Data.Semigroup ((<>))

import qualified Data.ByteString as B
import           Data.Semigroup ((<>))
import qualified Data.Yaml as Y
import qualified Data.Yaml.Builder as Builder
import qualified System.IO as I

data Options = Options {
  new       :: Bool,
  filename  :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optNew <*> optFilename
      optNew = O.switch
        (  O.long "new"
        <> O.short 'n'
        <> O.help "Assume pokemon without stats are newly caught or hatched")
      optFilename = O.argument O.str (O.metavar "FILENAME")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Calauate IVs for pokemon."
        <> O.header "header = Calauate IVs for pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main = Epic.catch (
  do
    options <- getOptions

    gameMaster <- do
      ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
      ioGameMaster

    myPokemon <- do
      ioMyPokemon <- MyPokemon.load $ filename options
      ioMyPokemon

    let new' = new options
    myNewPokemon <- mapM (updateStats gameMaster new') myPokemon
    B.putStr $ Builder.toByteString myNewPokemon
  )
  $ \ex -> I.hPutStrLn I.stderr $ ex

updateStats :: (Epic.MonadCatch m) => GameMaster -> Bool -> MyPokemon -> m MyPokemon
updateStats gameMaster new myPokemon = Epic.catch (
  do
    stats <- computeStats gameMaster new myPokemon
    return $ MyPokemon.setStats myPokemon stats
  )
  $ \ex -> Epic.fail $
      "Problem with " ++ MyPokemon.name myPokemon ++ ": " ++ ex

computeStats :: (Epic.MonadCatch m) => GameMaster -> Bool -> MyPokemon -> m [Stats]
computeStats gameMaster new myPokemon = do
  pokemonBase <- GameMaster.getPokemonBase gameMaster $ MyPokemon.species myPokemon
  possibleLevels <- GameMaster.getLevelsForStardust gameMaster
    $ MyPokemon.stardust myPokemon
  possibleIvs <- do
    appraisal <- Appraisal.new $ MyPokemon.appraisal myPokemon
    return $ Appraisal.possibleIvs appraisal
  possibleStats <- do
    let allStats = [Stats.new level attack defense stamina |
          level <- possibleLevels,
          (attack, defense, stamina) <- possibleIvs]
        isWholeLevel s =
          let level = Stats.level s
          in fromIntegral (floor level) == level
        allStats' = if new && (Maybe.isNothing $ MyPokemon.stats myPokemon)
          then filter isWholeLevel allStats
          else allStats
        statsMatchMyPokemon stats =
          MyPokemon.hp myPokemon ==
            Calc.hp gameMaster pokemonBase stats &&
          MyPokemon.cp myPokemon ==
            Calc.cp gameMaster pokemonBase stats
        statsThatMatchMyPokemon = filter statsMatchMyPokemon allStats'
    case statsThatMatchMyPokemon of
      [] -> Epic.fail "No possible ivs"
      matchingStats -> return $ do
        case MyPokemon.stats myPokemon of
          Nothing -> matchingStats
          Just currentStats ->
            filter (\matching ->
              any (\current ->
                let ivs = sequence [Stats.attack, Stats.defense, Stats.stamina]
                in ivs matching == ivs current)
              currentStats)
            matchingStats
  case possibleStats of
    [] -> Epic.fail "No possible remaining ivs"
    _ -> return possibleStats
