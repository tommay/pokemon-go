{-# LANGUAGE TupleSections #-}

module Main where

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified MakePokemon
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import qualified Util

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import           Control.Monad (join, forM, liftM)
import qualified Data.ByteString as B
import qualified Data.Yaml.Builder as Builder
import qualified System.Exit as Exit

data Options = Options {
  maybeFilename :: Maybe String,
  sortByAttack :: Bool
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilename <*> optSortByAttack
      optFilename = O.optional $ O.argument O.str (O.metavar "FILENAME")
      optSortByAttack = O.switch
        (  O.long "attack"
        <> O.short 'a'
        <> O.help "Sort by attack")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Sort a pokemon file by cp")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main :: IO ()
main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      myPokemon <- join $ MyPokemon.load $ maybeFilename options
      let getSortKey = if sortByAttack options
            then getAttack gameMaster
            else pure . fromIntegral . MyPokemon.cp
      augmented <- zipMapM getSortKey myPokemon
      let mySortedPokemon = reverse $ map snd $ Util.sortWith fst augmented
      B.putStr $ Builder.toByteString mySortedPokemon
  )
  $ Exit.die

getAttack :: Epic.MonadCatch m => GameMaster -> MyPokemon -> m Float
getAttack gameMaster myPokemon = do
  pokemon <- head <$> MakePokemon.makePokemon gameMaster myPokemon
  return $ Pokemon.attack pokemon

zipMapM :: Monad m => (a -> m b) -> [a] -> m [(b, a)]
zipMapM fn list = forM list $ \a -> (,a) <$> fn a
-- zipMapM fn list = forM list $ \a -> (\b -> (b, a)) <$> fn a
{-
zipMapM fn list = forM list $ \a -> do
  b <- fn a
  return $ (b, a)
-}
