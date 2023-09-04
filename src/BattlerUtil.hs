{-# LANGUAGE OverloadedStrings #-}

module BattlerUtil (
  Battler,
  Level (Normal),
  new,
  species,
  level,
  parseBattler,
  optParseBattler,
  makeBattlerVariants,
  makeRaidBossForTier,
  setLevel,
  getMatchingMove,
) where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))

import qualified Discounts
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified MakePokemon
import qualified Move
import           Move (Move)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified Util

import           Control.Applicative (optional, some, many)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Text.Printf as Printf

import qualified Debug as D

data Battler = Battler {
  species :: String,
  level :: Level,
  maybeQuickName :: Maybe String,
  maybeChargeName :: Maybe String
} deriving (Show)

data Level = Normal IVs | RaidBoss Int
  deriving (Show)

new = Battler

optParseBattler :: O.ReadM Battler
optParseBattler = O.eitherReader parseBattler

parseBattler :: String -> Either String Battler
parseBattler string =
  let attoParseBattler = do
        species <- some $ Atto.satisfy $ Atto.notInClass ":/"
        level <- optional $ do
          Atto.char ':'
          attoParseIVs <|> attoParseRaidBoss
        maybeQuickName <- optional $ do
          Atto.char ':'
          some $ Atto.notChar '/'
        maybeChargeName <- optional $ do
          Atto.char '/'
          some $ Atto.anyChar
        Atto.endOfInput
        return $ Battler {
          species = species,
          level = Maybe.fromMaybe (Normal IVs.defaultIVs) level,
          maybeQuickName = maybeQuickName,
          maybeChargeName = maybeChargeName
          }
      attoParseIVs = attoParseLevelAndIVs <|> attoParseLevel
      attoParseLevelAndIVs = do
        (level, _) <- Atto.match $ do
          Atto.decimal
          optional $ Atto.string ".5"
        let level' = read $ Text.unpack level
        Atto.char '/'
        attack <- Atto.decimal
        Atto.char '/'
        defense <- Atto.decimal
        Atto.char '/'
        stamina <- Atto.decimal
        return $ Normal $ IVs.new level' attack defense stamina
      attoParseLevel = do
        (level, _) <- Atto.match $ do
          Atto.decimal
          optional $ Atto.string ".5"
        let level' = read $ Text.unpack level
        return $ Normal $ IVs.setLevel IVs.defaultIVs level'
      attoParseRaidBoss = do
        Atto.satisfy $ Atto.inClass "rt"    -- r for raid, t for tier
        (raidLevel, _) <- Atto.match Atto.decimal
        return $ RaidBoss $ read $ Text.unpack raidLevel
  in case Atto.parseOnly attoParseBattler (Text.pack string) of
    Left _ ->
      Left $ "`" ++ string ++ "' should look like SPECIES[:LEVEL] or SPECIES:[rRAID-:LEVEL]"
    Right battler -> Right battler

makeBattlerVariants :: Epic.MonadCatch m => GameMaster -> Battler -> m [Pokemon]
makeBattlerVariants gameMaster battler = do
  let species = BattlerUtil.species battler
  base <- GameMaster.getPokemonBase gameMaster species
  let getMoves moveType getMovesFunc getMaybeAbbrev = do
        let moves = getMovesFunc base
        case getMaybeAbbrev battler of
          Nothing -> return moves
          Just abbrev -> do
            move <- getMatchingMove abbrev moves moveType species
            return [move]
  quickMoves <- getMoves "quick" PokemonBase.quickMoves
    BattlerUtil.maybeQuickName
  chargeMoves <- getMoves "charge" PokemonBase.chargeMoves
    BattlerUtil.maybeChargeName
  return $ case BattlerUtil.level battler of
    Normal ivs ->
      MakePokemon.makeForWhatevers
        gameMaster
        ivs
        (PokemonBase.species base)
        base
        quickMoves
        chargeMoves
    RaidBoss raidLevel ->
      makeRaidBossForMoves gameMaster raidLevel base quickMoves chargeMoves

-- We have to check for a move matching abbrev exactly before checking
-- with matchesAbbrevInsensitive, otherwise the abbrev "charm" will be
-- considered an abbreviation for both "charm" and "charge beam".
-- This is no longer true since abbreviation matching is done differently
-- but it still seems like a good idea if a move name ever becomes a prefix
-- of another move name.
--
getMatchingMoves :: String -> [Move] -> [Move]
getMatchingMoves abbrev moves =
  case filter ((== Util.toLower abbrev) . Util.toLower . Move.name) moves of
    [move] -> [move]
    _ ->
      let nameMatches = Util.matchesAbbrevInsensitive abbrev . Move.name
      in filter nameMatches moves

getMatchingMove :: Epic.MonadCatch m => String -> [Move] -> String -> String -> m Move
getMatchingMove abbrev moves moveType species =
  let (legacyMoves, normalMoves) = (List.partition Move.isLegacy) moves
      makeMovesString moves = List.intercalate "\n" $
        map (("  " ++) . Move.name) moves
      allMoves = "Available moves:\n" ++ makeMovesString normalMoves ++
        "\nLegacy moves:\n" ++ makeMovesString legacyMoves
  in case getMatchingMoves abbrev moves of
       [move] ->
         if not $ Move.isLegacy move
           then return move
           else Epic.fail $
             Printf.printf "%s is a legacy move\n" (Move.name move) ++ allMoves
       matchingMoves ->
         let howMany =
               if null matchingMoves then "no" else "multiple" :: String
         in Epic.fail $
              Printf.printf "%s has %s %s moves matching `%s'\n"
                species howMany moveType abbrev ++ allMoves

makeRaidBossForTier :: GameMaster -> Int -> PokemonBase -> [Pokemon]
makeRaidBossForTier gameMaster raidLevel base =
  makeRaidBossForMoves gameMaster raidLevel base
    (PokemonBase.quickMoves base) (PokemonBase.chargeMoves base)

-- https://pokemongo.gamepress.gg/how-raid-boss-cp-calculated
-- This is more to the point.  The CP calculations don't actually
-- match the real stats:
-- https://www.reddit.com/r/pokebattler/comments/6jvzqm/motivation_for_flat_600150030007500_hp/
-- XXX The stamina/hp works for the (fictitious) CP formula but may be
-- slightly too high, at least for t3.

makeRaidBossForMoves :: GameMaster -> Int -> PokemonBase -> [Move] -> [Move] -> [Pokemon]
makeRaidBossForMoves gameMaster raidLevel base quickMoves chargeMoves =
  let hp = case raidLevel of
        1 -> 600
        2 -> 1800
        3 -> 3600
        4 -> 9000
        5 -> 15000
        6 -> 22500 -- "Tier 6" for mewtwo, darkrai, mega legendaries.
        _ -> error "Raid level must be 1, 2, 3, 4, 5, or 6"
      makeStat baseFunc = fromIntegral $ baseFunc base + 15
      makePokemon quickMove chargeMove =
        Pokemon.new
          (PokemonBase.species base)
          base
          IVs.null     -- not used
          (makeStat PokemonBase.attack)
          (makeStat PokemonBase.defense)
          -- Defender's hp is doubled, but that doesn't seem to be
          -- the case for raid bosses, so here we halve the stamina to
          -- compensate for the doubling.
          (hp / 2)
          quickMove
          chargeMove
          Discounts.noDiscounts
      notLegacy = not . Move.isLegacy   
  in [makePokemon quickMove chargeMove |
       quickMove <- quickMoves, chargeMove <- chargeMoves,
       -- Raid bosses do not use legacy moves.
       notLegacy quickMove, notLegacy chargeMove]

setLevel :: Float -> Battler -> Battler
setLevel level battler =
  case BattlerUtil.level battler of
    Normal ivs ->
      battler { level = Normal $ IVs.setLevel ivs level }
    _ -> error $ "Can't set level of " ++ show battler

fromJustOrFail :: Epic.MonadCatch m => Maybe a -> String -> m a
fromJustOrFail maybe string =
  case maybe of
    Nothing -> Epic.fail string
    Just a -> return a
