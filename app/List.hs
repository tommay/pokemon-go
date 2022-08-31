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
  evolve :: Bool,
  minCp :: Int,
  allowedTypes :: [String],
  excludedTypes :: [String]
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLittle <*> optEvolve <*> optMinCp <*>
        optAllowedTypes <*> optExcludedTypes
      optLittle = O.switch
        (  O.long "little"
        <> O.short 'l'
        <> O.help "Show pokemon eligible for Little Cup and similar")
      optEvolve = O.switch
        (  O.long "evolve"
        <> O.short 'e'
        <> O.help "Show pokemon which have evolved and can evolve again")
      optMinCp =
            O.flag' 1400 (
              O.short 'g' <>
              O.long "great" <>
              O.help "great league")
        <|> O.flag' 2300 (
              O.short 'u' <>
              O.long "ultra" <>
              O.help "ultra league")
        <|> O.flag' 2700 (
              O.short 'm' <>
              O.long "master" <>
              O.help "master league")
        <|> O.option O.auto (
              O.long "mincp" <>
              O.short 'c' <>
              O.metavar "N" <>
              O.help "Minimum cp")
        <|> pure 0
      optAllowedTypes = (O.many . O.strOption)
        (  O.long "type"
        <> O.short 't'
        <> O.metavar "TYPE"
        <> O.help "Include only the specified type(s).")
      optExcludedTypes = (O.many . O.strOption)
        (  O.long "not"
        <> O.short 'n'
        <> O.metavar "NOT-TYPE"
        <> O.help "Exclude the specified type(s).")
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
          isMiddle middleRequired = if middleRequired
            then PokeUtil.isMiddleEvolution gameMaster
            else const True
          typeIn types = any (`elem` types) . map Type.name . PokemonBase.types
          hasAllowedType allowedTypes = case allowedTypes of
            [] -> const True
            _ -> typeIn allowedTypes
          hasExcludedType excludedTypes = case excludedTypes of
            [] -> const False
            _ -> typeIn excludedTypes
          minCp' = if little options
            then 400
            else minCp options
          hasEnoughCp = (>= minCp') . maxCp gameMaster
      mapM_ putStrLn $
        map PokemonBase.species $
        filter hasEnoughCp $
        filter (isLittle $ little options) $
        filter (isMiddle $ evolve options) $
        filter (hasAllowedType $ allowedTypes options) $
        filter (not . (hasExcludedType $ excludedTypes options)) $
        GameMaster.allPokemonBases gameMaster
    )
    $ Exit.die

maxCp :: GameMaster -> PokemonBase -> Int
maxCp gameMaster base =
  let maxIVs = IVs.new 40 15 15 15
  in Calc.cp gameMaster base maxIVs
