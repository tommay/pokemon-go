{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import           Move (Move)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Type
import           Type (Type)

import qualified Debug

import           Control.Monad (join, forM, forM_)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

--data Options = Options {
--  attacker :: String,
--  defender :: Maybe String
--}

--getOptions :: GameMaster -> IO Options
--getOptions gameMaster =
--  let opts = Options <$> optAttacker <*> optDefender
--      allSpecies = GameMaster.allSpecies gameMaster
--      optAttacker = O.strArgument
--        (  (O.metavar "ATTACKER")
--        <> O.completeWith allSpecies)
--      optDefender = O.optional $ O.strArgument
--        (  (O.metavar "DEFENDER")
--        <> O.completeWith allSpecies)
--      options = O.info (opts <**> O.helper)
--        (  O.fullDesc
--        <> O.progDesc "Rate movesets by spamminess.")
--      prefs = O.prefs O.showHelpOnEmpty
--  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      gameMaster <- join $ GameMaster.load
--      options <- getOptions gameMaster
      mapM_ (putStrLn . PokemonBase.species) $
        GameMaster.allPokemonBases gameMaster
    )
    $ Exit.die
