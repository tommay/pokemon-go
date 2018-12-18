-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import qualified PokemonBase
import qualified Type

import           Control.Monad (join, forM_)
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  species :: String,
  level :: Float,
  attack :: Int
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optSpecies <*> optLevel <*> optAttack
      optSpecies = O.strArgument (O.metavar "SPECIES")
      optLevel = O.argument O.auto (O.metavar "LEVEL")
      optAttack = O.argument O.auto (O.metavar "ATTACK")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc
             "Show how effective (or not) move types are against a pokemon")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      base <- GameMaster.getPokemonBase gameMaster $ species options
      let cpMultiplier = GameMaster.getCpMultiplier gameMaster (level options)
          attackIv = attack options
          absoluteAttack =
            (fromIntegral $ PokemonBase.attack base + attackIv) * cpMultiplier
      putStrLn $ show absoluteAttack
    )
    $ Exit.die
