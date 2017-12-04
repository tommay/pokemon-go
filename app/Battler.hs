{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Action
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
import qualified Mythical
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
  defender :: String
}

defaultLevel = 20

getOptions :: IO Options
getOptions = do
  let opts = Options <$> optDefender
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

      defenders <- makeWithAllMovesetsFromSpecies gameMaster (defender options)

      attackers <- do
        mythicalMap <- do
          ioMythicalMap <- Mythical.load "mythical.yaml"
          ioMythicalMap
        let notMythical = not . Mythical.isMythical mythicalMap . PokemonBase.species
            level = 20
        return $
          concat $ map (makeWithAllMovesetsFromBase gameMaster level)
            $ filter notMythical
            $ filter (not . PokemonBase.hasEvolutions)
                (GameMaster.allPokemonBases gameMaster)

      let rnd = Random.mkStdGen 23

      let battles = [Writer.runWriter $
              Battle.runBattle $ Battle.init rnd attacker defender |
            defender <- defenders, attacker <- attackers]
          battles' = map fst battles
          sorted = reverse $
            List.sortBy (compareWith Battle.damageInflicted) battles'

      mapM_ (putStrLn . showBattle) sorted
    )
    $ \ex -> I.hPutStrLn I.stderr ex

{-
gyarados quick/charge defends against
  order by damage inflicted
  pidgey quick/charge dps damage inflicted
  pidgey quick/charge dps damage inflicted
  caterpie quick/charge dps damage inflicted
  caterpie quick/charge dps damage inflicted
-}

showBattle :: Battle -> String
showBattle this =
  let defender = showPokemon $ Defender.pokemon $ Battle.defender this
      attacker = showPokemon $ Attacker.pokemon $ Battle.attacker this
      dps = Battle.dps this
      damageInflicted = Battle.damageInflicted this
  in Printf.printf "%-35s %-35s: %.1f %d" defender attacker dps damageInflicted

showPokemon :: Pokemon -> String
showPokemon pokemon =
  Printf.printf "%s %s / %s" (Pokemon.species pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)

compareWith :: Ord b => (a -> b) -> a -> a -> Ordering
compareWith f first second =
  f first `compare` f second

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
      (quickMove, chargeMove) <-
        PokemonBase.moveSets base]
