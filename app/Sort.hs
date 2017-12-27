module Main where

import qualified Epic
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Util

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import           Control.Monad (join)
import qualified Data.ByteString as B
import qualified Data.Yaml.Builder as Builder
import qualified System.IO as I

data Options = Options {
  filename :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilename
      optFilename = O.argument O.str (O.metavar "FILENAME")
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
      myPokemon <- join $ MyPokemon.load $ filename options
      let mySortedPokemon = reverse $ Util.sortWith MyPokemon.cp myPokemon
      B.putStr $ Builder.toByteString mySortedPokemon
  )
  $ I.hPutStrLn I.stderr
