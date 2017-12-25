{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}

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
import qualified Log
import           Log (Log)
import qualified Logger
import           Logger (Logger)
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

import           Control.Applicative (optional, some)
import           Control.Monad (join)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified System.IO as I
import qualified Text.Printf as Printf

defaultLevel = 20

data Battler = Battler String Float
  deriving (Show)

data Options = Options {
  maybeWeather :: Maybe Weather,
  attacker :: Battler,
  defender :: Battler
}

parseBattler :: O.ReadM Battler
parseBattler = O.eitherReader $ \s ->
  let attoParseBattler = do
        battler <- some $ Atto.notChar ':'
        level <- optional $ do
          Atto.char ':'
          (level, _) <- Atto.match $ do
            Atto.decimal
            optional $ Atto.string ".5"
          return $ read $ Text.unpack level
        Atto.endOfInput
        return $ Battler battler (Maybe.fromMaybe defaultLevel level)
  in case Atto.parseOnly attoParseBattler (Text.pack s) of
    Left _ ->
      Left $ "`" ++ s ++ "' should look like SPECIES[:LEVEL]"
    Right battler -> Right battler

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
      optAttacker = O.argument parseBattler (O.metavar "ATTACKER[:LEVEL]")
      optDefender = O.argument parseBattler (O.metavar "DEFENDER[:LEVEL]")
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
        let Battler species level = attacker options
        in makeWithAllMovesetsFromSpecies gameMaster species level

      defenderVariants <-
        let Battler species level = defender options
        in makeWithAllMovesetsFromSpecies gameMaster species level

      let battleLoggers =
            [Battle.runBattle $ Battle.init weatherBonus attacker defender |
              attacker <- attackerVariants, defender <- defenderVariants]
          battleResults = map Logger.runLogger battleLoggers

      putStrLn $ List.intercalate "\n" $ map showBattle battleResults
    )
    $ I.hPutStrLn I.stderr

tell :: a -> Logger a ()
tell = Logger.log

showBattle :: (Battle, [Log Battle]) -> String
showBattle (battle, logs) = 
  List.intercalate "\n" $
  Logger.execLogger (
    do
      tell $ showPokemon $ Attacker.pokemon $ Battle.attacker battle
      tell $ showPokemon $ Defender.pokemon $ Battle.defender battle
      mapM_ (tell . showLog) logs)

showLog :: Log Battle -> String
showLog log =
  let battle = Log.state log
      when = Battle.secondsElapsed battle
      attacker = Battle.attacker battle
      defender = Battle.defender battle
  in Printf.printf "%.3f: %3d %3d - %3d %3d: %s"
       when
       (Attacker.hp attacker) (Attacker.energy attacker)
       (Defender.hp defender) (Defender.energy defender)
       (Log.what log)

showPokemon :: Pokemon -> String
showPokemon pokemon =
  Printf.printf "%s %s / %s"
    (Pokemon.species pokemon)
    (Move.name $ Pokemon.quick pokemon)
    (Move.name $ Pokemon.charge pokemon)

makeWithAllMovesetsFromSpecies :: Epic.MonadCatch m =>
    GameMaster -> String -> Float -> m [Pokemon]
makeWithAllMovesetsFromSpecies gameMaster species level = do
  base <- GameMaster.getPokemonBase gameMaster species
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
