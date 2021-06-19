{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import qualified Move
import           Move (Move)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Type
import           Type (Type)
import qualified PokeUtil

import qualified Debug

import           Control.Monad (join, forM, forM_)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  little :: Bool,
  great :: Bool,
  allowedTypes :: [String]
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLittle <*> optGreat <*> optAllowedTypes
      optLittle = O.switch
        (  O.long "little"
        <> O.short 'l'
        <> O.help "Show pokemon eligible for Little Cup and similar")
      optGreat = O.switch
        (  O.long "great"
        <> O.short 'g'
        <> O.help "Show pokemon with enough cp for great league")
      optAllowedTypes = (O.many . O.strOption)
        (  O.long "type"
        <> O.short 't'
        <> O.metavar "TYPE"
        <> O.help "Include only the specified types.")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "List some or all pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      gameMaster <- join $ GameMaster.load
      options <- getOptions
      let isLittle littleRequired = if littleRequired
            then PokeUtil.isFirstEvolution gameMaster
            else const True
          hasAllowedType allowedTypes = case allowedTypes of
            [] -> const True
            _ -> any ((`elem` allowedTypes) . Type.name) . PokemonBase.types
          hasEnoughCp great = if great
            then (>= 1430) . maxCp gameMaster
            else const True
      mapM_ putStrLn $
        map PokemonBase.species $
        filter (hasEnoughCp $ great options) $
        filter (isLittle $ little options) $
        filter (hasAllowedType $ allowedTypes options) $
        GameMaster.allPokemonBases gameMaster
    )
    $ Exit.die

maxCp :: GameMaster -> PokemonBase -> Int
maxCp gameMaster base =
  let maxIVs = IVs.new 40 15 15 15
  in Calc.cp gameMaster base maxIVs
