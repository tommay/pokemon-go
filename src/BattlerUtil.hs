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
import qualified Move
import           Move (Move)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)

import           Control.Applicative (optional, some, many)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Text.Printf as Printf

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
          many $ Atto.notChar '/'
        maybeChargeName <- optional $ do
          Atto.char '/'
          many $ Atto.anyChar
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
makeBattlerVariants gameMaster battler = do
  let species = BattlerUtil.species battler
  base <- GameMaster.getPokemonBase gameMaster species
  let maybeQuickName = BattlerUtil.maybeQuickName battler
      maybeChargeName = BattlerUtil.maybeChargeName battler
      nameMatches abbrev move = matchesAnchored (Move.name move) abbrev
      quickMoves = PokemonBase.quickMoves base
      chargeMoves = PokemonBase.chargeMoves base
  quickMoves <- do
    case maybeQuickName of
      Nothing -> return quickMoves
      Just quickName ->
        case filter (nameMatches quickName) quickMoves of
          [val] -> return [val]
          val -> Epic.fail $
            matchingMovesFail val "quick" species
              (Maybe.fromJust maybeQuickName)
              (PokemonBase.quickMoves base)
  chargeMoves <- do
    case maybeChargeName of
      Nothing -> return chargeMoves
      Just chargeName ->
        case filter (nameMatches chargeName) chargeMoves of
          [val] -> return [val]
          val -> Epic.fail $
            matchingMovesFail val "charge" species
              (Maybe.fromJust maybeChargeName)
              (PokemonBase.chargeMoves base)
  return $ case BattlerUtil.level battler of
    Level level ->
      makeForMoves gameMaster level base quickMoves chargeMoves
    RaidBoss raidLevel ->
      makeRaidBossForMoves gameMaster raidLevel base quickMoves chargeMoves

matchingMovesFail :: [a] -> String -> String -> String -> [Move] -> String
matchingMovesFail list moveType species moveName moves =
  let howMany = if null list then "no" else "multiple" :: String
  in Printf.printf ("%s has %s %s moves matching `%s'\n" ++
       "Available %s moves:\n%s")
       species howMany moveType moveName moveType
       (List.intercalate "\n" $ map (("  " ++) . Move.name) moves)

makeForMoves :: GameMaster -> Float -> PokemonBase -> [Move] -> [Move] -> [Pokemon]
makeForMoves gameMaster level base quickMoves chargeMoves =
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
       quickMove <- quickMoves, chargeMove <- chargeMoves]

-- https://pokemongo.gamepress.gg/how-raid-boss-cp-calculated

makeRaidBossForMoves :: GameMaster -> Int -> PokemonBase -> [Move] -> [Move] -> [Pokemon]
makeRaidBossForMoves gameMaster raidLevel base quickMoves chargeMoves =
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
       quickMove <- quickMoves, chargeMove <- chargeMoves]

matchesAnchored :: String -> String -> Bool
matchesAnchored _ [] = True
matchesAnchored [] _ = False
matchesAnchored (s1:ss) (p1:pp) =
  s1 == p1 && matches ss pp

matches :: String -> String -> Bool
matches _ [] = True
matches [] _ = False
matches (s1:ss) p@(p1:pp) =
  if s1 == p1
    then matches ss pp
    else matches ss p
