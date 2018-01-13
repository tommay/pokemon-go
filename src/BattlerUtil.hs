{-# LANGUAGE OverloadedStrings #-}

module BattlerUtil (
  Battler,
  Level (Level),
  species,
  level,
  parseBattler,
  makeBattlerVariants,
) where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)

import           Control.Applicative (optional, some)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

data Battler = Battler {
  species :: String,
  level :: Level,
  maybeQuickName :: Maybe String,
  maybeChargeName :: Maybe String
} deriving (Show)

data Level = Level Float | RaidBoss Int
  deriving (Show)

parseBattler :: Level -> O.ReadM Battler
parseBattler defaultLevel = O.eitherReader $ \s ->
  let attoParseBattler = do
        battler <- some $ Atto.satisfy $ Atto.notInClass ":/"
        level <- optional $ do
          Atto.char ':'
          attoParseLevel <|> attoParseRaidBoss
        maybeQuickName <- optional $ do
          Atto.char ':'
          some $ Atto.notChar '/'
        maybeChargeName <- optional $ do
          Atto.char '/'
          some $ Atto.anyChar
        Atto.endOfInput
        return $ Battler {
          species = battler,
          level = Maybe.fromMaybe defaultLevel level,
          maybeQuickName = maybeQuickName,
          maybeChargeName = maybeChargeName
          }
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

makeBattlerVariants :: Epic.MonadCatch m => GameMaster -> Battler -> m [Pokemon]
makeBattlerVariants gameMaster battler =
  let species = BattlerUtil.species battler
      level = BattlerUtil.level battler
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
