module Main where

import qualified Calc
import qualified Epic
import qualified IVs
import           IVs (IVs)
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified TweakLevel

import qualified Options.Applicative as O
import           Options.Applicative ((<**>))
import           Data.Semigroup ((<>))

import           Control.Monad (join)
import qualified Data.ByteString as B
import qualified Data.Yaml.Builder as Builder
import qualified System.IO as I

data Options = Options {
  maybeTweakLevel :: Maybe (Float -> Float),
  maybeFilename  :: Maybe String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optMaybeTweakLevel <*> optFilename
      optMaybeTweakLevel = TweakLevel.optMaybeTweakLevel
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

    myNewPokemon <-
      mapM (updateFromIVs gameMaster (maybeTweakLevel options)) myPokemon
    B.putStr $ Builder.toByteString myNewPokemon
  )
  $ I.hPutStrLn I.stderr

updateFromIVs :: (Epic.MonadCatch m) =>
  GameMaster -> Maybe (Float -> Float) -> MyPokemon -> m MyPokemon
updateFromIVs gameMaster maybeTweakLevel myPokemon = do
  pokemonBase <-
    GameMaster.getPokemonBase gameMaster $ MyPokemon.species myPokemon
  let failNoIVs = Epic.fail $ MyPokemon.name myPokemon ++ ": no ivs"
  case MyPokemon.ivs myPokemon of
    Nothing -> failNoIVs
    Just [] -> failNoIVs
    Just (ivs:_) ->
      let ivs' = case maybeTweakLevel of
            Nothing -> ivs
            Just tweakLevel -> IVs.setLevel ivs $ tweakLevel $ IVs.level ivs
          cp = Calc.cp gameMaster pokemonBase ivs'
          hp = Calc.hp gameMaster pokemonBase ivs'
          stardust =
            GameMaster.getStardustForLevel gameMaster $ IVs.level ivs'
      in return $ myPokemon {
        MyPokemon.ivs = case maybeTweakLevel of
          Nothing -> MyPokemon.ivs myPokemon
          _ -> Just [ivs'],
        MyPokemon.cp = cp,
        MyPokemon.hp = hp,
        MyPokemon.stardust = stardust
        }
