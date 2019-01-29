module Main where

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MakePokemon
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import qualified PokeUtil
import qualified Util

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import           Control.Monad (join, (<=<))
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Yaml.Builder as Builder
import qualified System.Exit as Exit

data Options = Options {
  maybeFilename :: Maybe String,
  sortByAttack :: Bool,
  evolve :: OptEvolve
}

data OptEvolve = Evolve | EvolveTo String | NoEvolve

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilename <*> optSortByAttack <*> optEvolve
      optFilename = O.optional $ O.argument O.str (O.metavar "FILENAME")
      optSortByAttack = O.switch
        (  O.long "attack"
        <> O.short 'a'
        <> O.help "Sort by attack")
      optEvolve =
        let optEvolve' = O.flag' Evolve
              (  O.long "evolve"
              <> O.short 'E'
              <> O.help "Sort by evolved values")
            optEvolveTo = EvolveTo <$> O.strOption
              (  O.long "evolveTo"
              <> O.short 'e'
              <> O.help "Sort by evolved values of a given evolution")
        in optEvolve' <|> optEvolveTo <|> pure NoEvolve
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Sort a pokemon file by cp or attack")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main :: IO ()
main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      myPokemon <- join $ MyPokemon.load $ maybeFilename options
      let evolveFunc = case evolve options of
            Evolve -> PokeUtil.evolveFully gameMaster Nothing
            EvolveTo target -> PokeUtil.evolveFully gameMaster (Just target)
            NoEvolve -> return
      let getSortKey = if sortByAttack options
            then getAttack gameMaster
            else pure . fromIntegral . MyPokemon.cp
      augmented <- Util.augmentM (getSortKey <=< evolveFunc) myPokemon
      let mySortedPokemon = reverse $ map snd $ List.sortOn fst augmented
      B.putStr $ Builder.toByteString mySortedPokemon
  )
  $ Exit.die

getAttack :: Epic.MonadCatch m => GameMaster -> MyPokemon -> m Float
getAttack gameMaster myPokemon = do
  pokemon <- head <$> MakePokemon.makePokemon gameMaster myPokemon
  return $ Pokemon.attack pokemon
