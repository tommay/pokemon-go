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
  setLevel,
  getMatchingMove,
) where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))

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

data Level = Normal IVs | RaidBoss Int
  deriving (Show)

new = Battler

optParseBattler :: IVs -> O.ReadM Battler
optParseBattler = O.eitherReader . parseBattler

parseBattler :: IVs -> String -> Either String Battler
parseBattler defaultIVs string =
  let attoParseBattler = do
        battler <- some $ Atto.satisfy $ Atto.notInClass ":/"
        level <- optional $ do
          Atto.char ':'
          attoParseIVs <|> attoParseRaidBoss
        maybeQuickName <- optional $ do
          Atto.char ':'
          many $ Atto.notChar '/'
        maybeChargeName <- optional $ do
          Atto.char '/'
          many $ Atto.anyChar
        Atto.endOfInput
        return $ Battler {
          species = battler,
          level = Maybe.fromMaybe (Normal defaultIVs) level,
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
        return $ Normal $ IVs.setLevel defaultIVs level'
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
        [ivs]
        (PokemonBase.species base)
        base
        quickMoves
        chargeMoves
    RaidBoss raidLevel ->
      makeRaidBossForMoves gameMaster raidLevel base quickMoves chargeMoves

getMatchingMove :: Epic.MonadCatch m => String -> [Move] -> String -> String -> m Move
getMatchingMove abbrev moves moveType species =
  let nameMatches = Util.matchesAbbrevInsensitive abbrev . Move.name
  in case filter nameMatches moves of
       [move] -> return move
       val -> Epic.fail $ matchingMoveFail abbrev moves moveType species val

matchingMoveFail :: String -> [Move] -> String -> String -> [a] -> String
matchingMoveFail abbrev allMoves moveType species list =
  let howMany = if null list then "no" else "multiple" :: String
  in Printf.printf ("%s has %s %s moves matching `%s'\n" ++
       "Available %s moves:\n%s")
       species howMany moveType abbrev moveType
       (List.intercalate "\n" $ map (("  " ++) . Move.name) allMoves)

-- https://pokemongo.gamepress.gg/how-raid-boss-cp-calculated
-- This is more to the point.  The CP calculations don't actually
-- match the real stats:
-- https://www.reddit.com/r/pokebattler/comments/6jvzqm/motivation_for_flat_600150030007500_hp/
-- XXX The stamina/hp works for the (fictitious) CP formula but may be
-- slightly too high, at least for r3.

makeRaidBossForMoves :: GameMaster -> Int -> PokemonBase -> [Move] -> [Move] -> [Pokemon]
makeRaidBossForMoves gameMaster raidLevel base quickMoves chargeMoves =
  let -- These cpm were apparently sniffed off the wire and are the same
      -- ones used by https://pokemongo.gamepress.gg/breakpoint-calculator#/.
      -- With these values ./breakpoint gyarados:30/13/14/10:wf machamp:r3
      -- matches that site, but using getCpmForLevel gives the jump to
      -- 14 at level 28 whereas the site and the hardcoded numbers put
      -- it at level 27.5.
      -- getCpmForLevel = GameMaster.getCpMultiplier gameMaster
      (cpm, hp) = case raidLevel of
        1 -> (0.61, 600)
        2 -> (0.67, 1800)
        3 -> (0.73, 3600)
        4 -> (0.79, 9000)
        5 -> (0.79, 15000)
        6 -> (0.79, 18750)  -- XXX It remains to be seen whether "tier 6" has
                            -- increased, or will be used again, but the 1.2
                            -- multiplier for tiers 3-5 would make it 22500.
        _ -> error "Raid level must be 1, 2, 3, 4, or 5"
      makeStat baseFunc = (fromIntegral $ baseFunc base + 15) * cpm
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
