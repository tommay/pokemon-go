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
import qualified PokemonClass (PokemonClass(..))
import qualified PokeUtil
import qualified Type
import           Type (Type)
import qualified Util

import qualified Debug as D

import           Control.Monad (join, forM, forM_)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  maybeCup :: Maybe String,
  little :: Bool,
  evolve :: Bool,
  minCp :: Int,
  allowedTypes :: [String],
  excludedTypes :: [String],
  bannedPokemon :: [String],
  premier :: Bool
}

type Cup = Options

defaultCup = Options {
  maybeCup = Nothing,
  little = False,
  evolve = False,
  minCp = 0,
  allowedTypes = [],
  excludedTypes = [],
  bannedPokemon = [],
  premier = False
  }

cupMap :: HashMap String Options
cupMap = HashMap.fromList [
  ("fighting", defaultCup {
    allowedTypes = ["fighting"],
    excludedTypes = ["psychic"]
    }),
  ("little-jungle-remix", defaultCup {
    allowedTypes = ["normal", "grass", "electric", "poison", "ground",
      "flying", "bug", "dark"],
    bannedPokemon = ["skorupi", "cottonee", "ducklett",
      "shuckle", "smeargle", "salandit"]
    }),
  ("evolution", defaultCup { evolve = True }),
  ("psychic", defaultCup {
    allowedTypes = ["psychic"],
    bannedPokemon = ["mew"] }),
  ("weather", defaultCup { allowedTypes = ["fire", "water", "ice", "rock"] }),
  ("halloween", defaultCup {
    allowedTypes = ["poison", "dark", "ghost", "bug", "fairy"] }),
  ("willpower", defaultCup {
    allowedTypes = ["fighting", "psychic", "dark"],
    bannedPokemon = ["gardevoir"] })
  ]

getCup :: Epic.MonadCatch m => Options -> m Cup
getCup options = case maybeCup options of
  Nothing -> return options
  Just cupName -> case HashMap.lookup cupName cupMap of
    Nothing -> Epic.fail "oops"
    Just cup -> return cup

getOptions :: IO Options
getOptions =
  let opts = Options <$> optCup <*> optLittle <*> optEvolve <*> optMinCp <*>
        optAllowedTypes <*> optExcludedTypes <*> optBannedPokemon <*>
        optPremier
      optCup = O.optional $ O.strOption
        (  O.long "cup"
        <> O.short 'c'
        <> O.metavar "CUP"
        <> O.help "Include pokemon eligible for the given cup.")
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
      optBannedPokemon = (O.many . O.strOption)
        (  O.long "banned"
        <> O.short 'b'
        <> O.metavar "POKEMON"
        <> O.help "Banned pokemon to exclude.")
      optPremier = O.switch
        (  O.long "premier"
        <> O.short 'p'
        <> O.help "Show pokemon eligible for premier cups")
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
      options <- getCup options
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
          isBanned bannedPokemon pokemon =
            Util.toLower pokemon `elem` map Util.toLower bannedPokemon
          isPremier premierRequired =
            if premierRequired
              then (`notElem` [PokemonClass.Legendary, PokemonClass.Mythic]) . PokemonBase.pokemonClass
              else const True
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
        filter (not . isBanned (bannedPokemon options) . PokemonBase.species) $
        filter (isPremier $ premier options) $
        GameMaster.allPokemonBases gameMaster
    )
    $ Exit.die

maxCp :: GameMaster -> PokemonBase -> Int
maxCp gameMaster base =
  let maxIVs = IVs.new 40 15 15 15
  in Calc.cp gameMaster base maxIVs
