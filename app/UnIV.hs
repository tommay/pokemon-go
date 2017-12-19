module Main where

import qualified Calc
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified PokemonBase (PokemonBase)
import qualified Stats
import           Stats (Stats)

import qualified Options.Applicative as O
import           Options.Applicative ((<**>))
import           Data.Semigroup ((<>))

import           Control.Monad (join)
import qualified Data.ByteString as B
import           Data.Semigroup ((<>))
import qualified Data.Yaml.Builder as Builder
import qualified System.IO as I

data Options = Options {
  filename  :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilename
      optFilename = O.argument O.str (O.metavar "FILENAME")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Calculate pokemon values from IVs.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main = Epic.catch (
  do
    options <- getOptions

    gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

    myPokemon <- join $ MyPokemon.load $ filename options

    myNewPokemon <- mapM (updateFromStats gameMaster) myPokemon
    B.putStr $ Builder.toByteString myNewPokemon
  )
  $ I.hPutStrLn I.stderr

updateFromStats :: (Epic.MonadCatch m) => GameMaster -> MyPokemon -> m MyPokemon
updateFromStats gameMaster myPokemon = do
  pokemonBase <-
    GameMaster.getPokemonBase gameMaster $ MyPokemon.species myPokemon
  let failNoStats = Epic.fail $ MyPokemon.name myPokemon ++ ": no stats"
  case MyPokemon.stats myPokemon of
    Nothing -> failNoStats
    Just [] -> failNoStats
    Just (stats:_) ->
      let cp = Calc.cp gameMaster pokemonBase stats
          hp = Calc.hp gameMaster pokemonBase stats
          stardust =
            GameMaster.getStardustForLevel gameMaster $ Stats.level stats
      in return $ myPokemon {
        MyPokemon.cp = cp,
        MyPokemon.hp = hp,
        MyPokemon.stardust = stardust
        }
