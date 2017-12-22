{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Action
import           Action (Action)
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
import           Weather (Weather (..))

import           Control.Monad (join)
import qualified Control.Monad.Writer as Writer
import           Control.Monad.Writer (Writer)
import qualified Data.List as List
import qualified System.IO as I
import qualified Text.Printf as Printf

defaultLevel = 20

data Options = Options {
  maybeWeather :: Maybe Weather,
  attacker :: String,
  defender :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optWeather <*> optAttacker <*> optDefender
      optWeather = O.optional $
            O.flag' Clear (O.long "clear" <> O.help "weather is sunny/clear")
        <|> O.flag' Fog (O.long "fog")
        <|> O.flag' Overcast (O.long "overcast")
        <|> O.flag' PartlyCloudy (O.long "partlycloudy")
        <|> O.flag' Rainy (O.long "rainy")
        <|> O.flag' Snow (O.long "snow")
        <|> O.flag' Windy (O.long "windy")
      optAttacker = O.argument O.str (O.metavar "Attacker")
      optDefender = O.argument O.str (O.metavar "DEFENDER")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Battle some pokemon.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      let weatherBonus = case maybeWeather options of
            Just weather -> GameMaster.getWeatherBonus gameMaster weather
            Nothing -> const 1

      attackerVariants <-
        makeWithAllMovesetsFromSpecies gameMaster (attacker options)

      defenderVariants <-
        makeWithAllMovesetsFromSpecies gameMaster (defender options)

      let battleWriters =
            [Battle.runBattle $ Battle.init weatherBonus attacker defender |
              attacker <- attackerVariants, defender <- defenderVariants]
          battleResults = map Writer.runWriter battleWriters

      putStrLn $ List.intercalate "\n" $ map showBattle battleResults
    )
    $ I.hPutStrLn I.stderr

tell :: a -> Writer [a] ()
tell a =
  Writer.tell [a]

showBattle :: (Battle, [Action]) -> String
showBattle (battle, actions) = 
  List.intercalate "\n" $
  Writer.execWriter (
    do
      tell $ showPokemon $ Attacker.pokemon $ Battle.attacker battle
      tell $ showPokemon $ Defender.pokemon $ Battle.defender battle
      mapM_ (tell . showAction) actions)

showAction :: Action -> String
showAction action =
  Printf.printf "%5d: %3d %3d - %3d %3d: %s"
    (Action.when action)
    (Action.attackerHp action) (Action.attackerEnergy action)
    (Action.defenderHp action) (Action.defenderEnergy action)
    (Action.what action)

showPokemon :: Pokemon -> String
showPokemon pokemon =
  Printf.printf "%s %s / %s"
    (Pokemon.species pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)

makeWithAllMovesetsFromSpecies :: Epic.MonadCatch m =>
    GameMaster -> String -> m [Pokemon]
makeWithAllMovesetsFromSpecies gameMaster species = do
  base <- GameMaster.getPokemonBase gameMaster species
  return $ makeWithAllMovesetsFromBase gameMaster defaultLevel base

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
