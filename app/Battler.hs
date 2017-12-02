{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Attacker
import           Attacker (Attacker)
import qualified Battle
import           Battle (Battle)
import qualified Defender
import           Defender (Defender)
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import           Move (Move)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Mythical
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Type
import           Type (Type)

import           Control.Applicative (optional, some)
import qualified Control.Monad.Writer as Writer
--import qualified Data.Attoparsec.Text as AP
import qualified Data.List as List
import qualified Data.Maybe as Maybe
--import qualified Data.Set as Set
--import qualified Data.Text as Text
import qualified System.IO as I
import qualified System.Random as Random
import qualified Text.Printf as Printf
--import qualified Text.Regex as Regex

data Options = Options {
  attacker :: String,
  defender :: String
}

defaultLevel = 20

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optAttacker <*> optDefender
      optAttacker = O.argument O.str (O.metavar "ATTACKER")
      optDefender = O.argument O.str (O.metavar "DEFENDER")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Battle some pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- do
        ioGameMaster <- GameMaster.load "GAME_MASTER.yaml"
        ioGameMaster

      attackers <- makeWithAllMovesetsFromSpecies gameMaster (attacker options)
      defenders <- makeWithAllMovesetsFromSpecies gameMaster (defender options)

      let rnd = Random.mkStdGen 23

      let battles = [Writer.runWriter $
              Battle.runBattle $ Battle.init rnd attacker defender |
            attacker <- attackers, defender <- defenders]

      let (battle, actions) = head battles
      mapM_ (putStrLn . show) actions
    )
    $ \ex -> I.hPutStrLn I.stderr ex

makeWithAllMovesetsFromSpecies :: Epic.MonadCatch m =>
    GameMaster -> String -> m [Pokemon]
makeWithAllMovesetsFromSpecies gameMaster species = do
  base <- GameMaster.getPokemonBase gameMaster species
  let level = 20
  return $ makeWithAllMovesetsFromBase gameMaster level base

makeWithAllMovesetsFromBase :: GameMaster -> Float -> PokemonBase -> [Pokemon]
makeWithAllMovesetsFromBase gameMaster level base =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      makeStat baseStat = (fromIntegral baseStat + 11) * cpMultiplier
      makeAttacker quickMove chargeMove =
        Pokemon.new
          (PokemonBase.species base)
          (PokemonBase.species base)
          level
          (PokemonBase.types base)
          (makeStat $ PokemonBase.attack base)
          (makeStat $ PokemonBase.defense base)
          (makeStat $ PokemonBase.stamina base)
          quickMove
          chargeMove
          base
  in [makeAttacker quickMove chargeMove |
      quickMove <- PokemonBase.quickMoves base,
      chargeMove <- PokemonBase.chargeMoves base]
