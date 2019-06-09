-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil
import qualified Util

import           Control.Monad (join, forM_)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  calculateDefense :: Bool,
  species :: String,
  stats :: [(Float, Int)]
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optCalculateDefense <*> optSpecies <*> optStats
      optCalculateDefense = O.switch
        (  O.long "defense"
        <> O.short 'd'
        <> O.help "calculate defense instead of attack")
      optSpecies = O.strArgument (O.metavar "SPECIES")
      optStats = O.some $ (,) <$> optLevel <*> optAttack
      optLevel = O.argument O.auto (O.metavar "LEVEL")
      optAttack = O.argument O.auto (O.metavar "ATTACK")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc
             "Show a species' absolute attack for different levels/attackIvs")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      base <- GameMaster.getPokemonBase gameMaster (species options)
      let baseStat = if calculateDefense options
            then PokemonBase.defense base
            else PokemonBase.attack base
      let augmented = Util.augment (getStat gameMaster baseStat)
            (stats options)
          sorted = List.reverse $ List.sortOn snd augmented
      forM_ sorted $ \ ((level, attackIv), attack) ->
        Printf.printf "%s %d: %f\n"
          (PokeUtil.levelToString level) attackIv attack
    )
    $ Exit.die

getStat :: GameMaster -> Int -> (Float, Int) -> Float
getStat gameMaster baseStat (level, attackIv) =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster level
  in (fromIntegral $ baseStat + attackIv) * cpMultiplier
