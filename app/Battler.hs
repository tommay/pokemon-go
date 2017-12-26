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

defaultLevel = Level 20

data Battler = Battler String Level
  deriving (Show)

data Level = Level Float | RaidBoss Int
  deriving (Show)

data Options = Options {
  maybeWeather :: Maybe Weather,
  attacker :: Battler,
  defender :: Battler
}

parseBattler :: Level -> O.ReadM Battler
parseBattler defaultLevel = O.eitherReader $ \s ->
  let attoParseBattler = do
        battler <- some $ Atto.notChar ':'
        level <- optional $ do
          Atto.char ':'
          attoParseLevel <|> attoParseRaidBoss
        Atto.endOfInput
        return $ Battler battler (Maybe.fromMaybe defaultLevel level)
      attoParseLevel = do
        (level, _) <- Atto.match $ do
          Atto.decimal
          optional $ Atto.string ".5"
        return $ Level $ read $ Text.unpack level
      attoParseRaidBoss = do
        Atto.char 'r'
        (raidLevel, _) <- Atto.match Atto.decimal
        return $ RaidBoss $ read $ Text.unpack raidLevel
  in case Atto.parseOnly attoParseBattler (Text.pack s) of
    Left _ ->
      Left $ "`" ++ s ++ "' should look like SPECIES[:LEVEL] or SPECIES:[rRAID-:LEVEL]"
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
      optAttacker = O.argument
        (parseBattler defaultLevel) (O.metavar "ATTACKER[:LEVEL]")
      optDefender = O.argument
        (parseBattler defaultLevel) (O.metavar "DEFENDER[:LEVEL]")
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

      attackerVariants <- makeBattlerVariants gameMaster (attacker options)
      defenderVariants <- makeBattlerVariants gameMaster (defender options)

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

makeBattlerVariants :: Epic.MonadCatch m => GameMaster -> Battler -> m [Pokemon]
makeBattlerVariants gameMaster battler =
  let Battler species level = battler
  in case level of
    Level level ->
      makeWithAllMovesetsFromSpecies gameMaster species level
    RaidBoss raidLevel ->
      makeRaidBossWithAllMovesetsFromSpecies gameMaster species raidLevel

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

makeRaidBossWithAllMovesetsFromSpecies :: Epic.MonadCatch m =>
    GameMaster -> String -> Int -> m [Pokemon]
makeRaidBossWithAllMovesetsFromSpecies gameMaster species raidLevel = do
  base <- GameMaster.getPokemonBase gameMaster species
  return $ makeRaidBossWithAllMovesetsFromBase gameMaster raidLevel base

-- https://pokemongo.gamepress.gg/how-raid-boss-cp-calculated

makeRaidBossWithAllMovesetsFromBase gameMaster raidLevel base =
  let stamina = case raidLevel of
        1 -> 600
        2 -> 1800
        3 -> 3000
        4 -> 7500
        5 -> 12500
        _ -> error "Raid level must be 1, 2, 3, 4, or 5"
      makeStat baseStat = fromIntegral baseStat + 15
      makePokemon quickMove chargeMove =
        Pokemon.new
          (PokemonBase.species base)
          (PokemonBase.species base)
          0   -- level, not used
          (PokemonBase.types base)
          (makeStat $ PokemonBase.attack base)
          (makeStat $ PokemonBase.defense base)
          stamina
          quickMove
          chargeMove
          base
  in [makePokemon quickMove chargeMove |
      (quickMove, chargeMove) <-
        PokemonBase.moveSets base]
