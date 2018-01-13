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

import           Control.Applicative (optional, some)
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
makeBattlerVariants gameMaster battler = do
  let species = BattlerUtil.species battler
  base <- GameMaster.getPokemonBase gameMaster species
  let maybeQuickName = BattlerUtil.maybeQuickName battler
      maybeChargeName = BattlerUtil.maybeChargeName battler
      -- Check that first letter of each word in name matches the
      -- abbreviation, e.g., abbrevMatches "dazzling gleam" "dg" is
      -- True.
      abbrevMatches name abbrev =
        abbrev == (map (Char.toLower . head) $ words name)
      nameMatches maybeAbbrev move =
        and $ abbrevMatches (Move.name move) <$> maybeAbbrev
  moveSets <- do
    let moveSets = PokemonBase.moveSets base
    moveSets <- do
      case filter (\ (quick, _) -> nameMatches maybeQuickName quick)
          moveSets of
        [] -> Epic.fail $
          noMatchingMoves "quick" species (Maybe.fromJust maybeQuickName)
            (PokemonBase.quickMoves base)
        val -> return val
    moveSets <- do
      case filter (\ (_, charge) -> nameMatches maybeChargeName charge)
          moveSets of
        [] -> Epic.fail $
          noMatchingMoves "charge" species (Maybe.fromJust maybeChargeName)
            (PokemonBase.chargeMoves base)
        val -> return val
    return moveSets
  return $ case BattlerUtil.level battler of
    Level level ->
      makeForMoveSets gameMaster level base moveSets
    RaidBoss raidLevel ->
      makeRaidBossForMovesets gameMaster raidLevel base moveSets

noMatchingMoves :: String -> String -> String -> [Move] -> String
noMatchingMoves moveType species moveName moves =
  Printf.printf ("%s has no %s moves matching %s\n" ++
    "Available %s moves:\n%s")
    species moveType moveName moveType
    (List.intercalate "\n" $ map (("  " ++) . Move.name) moves)

makeForMoveSets :: GameMaster -> Float -> PokemonBase -> [(Move, Move)] -> [Pokemon]
makeForMoveSets gameMaster level base moveSets =
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
      (quickMove, chargeMove) <- moveSets]

-- https://pokemongo.gamepress.gg/how-raid-boss-cp-calculated

makeRaidBossForMovesets :: GameMaster -> Int -> PokemonBase -> [(Move, Move)] -> [Pokemon]
makeRaidBossForMovesets gameMaster raidLevel base moveSets =
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
      (quickMove, chargeMove) <- moveSets]
