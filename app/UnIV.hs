module Main where

import qualified Calc
import qualified Epic
import qualified IVs
import           IVs (IVs)
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified PokemonBase (PokemonBase)

import qualified Options.Applicative as O
import           Options.Applicative ((<**>))
import           Data.Semigroup ((<>))

import           Control.Monad (join)
import qualified Data.ByteString as B
import qualified Data.Yaml.Builder as Builder
import qualified System.IO as I

data Options = Options {
  maybeFilename  :: Maybe String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilename
      optFilename = O.optional $ O.argument O.str (O.metavar "FILENAME")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Calculate pokemon values from IVs.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main = Epic.catch (
  do
    options <- getOptions

    gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

    myPokemon <- join $ MyPokemon.load $ maybeFilename options

    myNewPokemon <- mapM (updateFromIVs gameMaster) myPokemon
    B.putStr $ Builder.toByteString myNewPokemon
  )
  $ I.hPutStrLn I.stderr

updateFromIVs :: (Epic.MonadCatch m) => GameMaster -> MyPokemon -> m MyPokemon
updateFromIVs gameMaster myPokemon = do
  pokemonBase <-
    GameMaster.getPokemonBase gameMaster $ MyPokemon.species myPokemon
  let failNoIVs = Epic.fail $ MyPokemon.name myPokemon ++ ": no ivs"
  case MyPokemon.ivs myPokemon of
    Nothing -> failNoIVs
    Just [] -> failNoIVs
    Just (ivs:_) ->
      let cp = Calc.cp gameMaster pokemonBase ivs
          hp = Calc.hp gameMaster pokemonBase ivs
          stardust =
            GameMaster.getStardustForLevel gameMaster $ IVs.level ivs
      in return $ myPokemon {
        MyPokemon.cp = cp,
        MyPokemon.hp = hp,
        MyPokemon.stardust = stardust
        }
