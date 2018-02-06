{-# LANGUAGE OverloadedStrings #-}

module BattlerUtil (
  Battler,
  Level (Level),
  new,
  species,
  level,
  parseBattler,
  makeBattlerVariants,
  setLevel,
) where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
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

data Level = Level IVs | RaidBoss Int
  deriving (Show)

new = Battler

parseBattler :: IVs -> O.ReadM Battler
parseBattler defaultIVs = O.eitherReader $ \s ->
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
          level = Maybe.fromMaybe (Level defaultIVs) level,
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
        return $ Level $ IVs.new level' attack defense stamina
      attoParseLevel = do
        (level, _) <- Atto.match $ do
          Atto.decimal
          optional $ Atto.string ".5"
        let level' = read $ Text.unpack level
        return $ Level $ IVs.setLevel defaultIVs level'
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
      nameMatches abbrev = Util.matchesAbbrevInsensitive abbrev . Move.name
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
    Level ivs ->
      makeForMoves gameMaster ivs base quickMoves chargeMoves
    RaidBoss raidLevel ->
      makeRaidBossForMoves gameMaster raidLevel base quickMoves chargeMoves

matchingMovesFail :: [a] -> String -> String -> String -> [Move] -> String
matchingMovesFail list moveType species moveName moves =
  let howMany = if null list then "no" else "multiple" :: String
  in Printf.printf ("%s has %s %s moves matching `%s'\n" ++
       "Available %s moves:\n%s")
       species howMany moveType moveName moveType
       (List.intercalate "\n" $ map (("  " ++) . Move.name) moves)

makeForMoves :: GameMaster -> IVs -> PokemonBase -> [Move] -> [Move] -> [Pokemon]
makeForMoves gameMaster ivs base quickMoves chargeMoves =
  let level = IVs.level ivs
      cpMultiplier = GameMaster.getCpMultiplier gameMaster level
      makeStat baseFunc ivFunc =
        (fromIntegral $ baseFunc base + ivFunc ivs) * cpMultiplier
      makeAttacker quickMove chargeMove =
        Pokemon.new
          (PokemonBase.species base)
          (PokemonBase.species base)
          level
          (PokemonBase.types base)
          (makeStat PokemonBase.attack IVs.attack)
          (makeStat PokemonBase.defense IVs.defense)
          (makeStat PokemonBase.stamina IVs.stamina)
          quickMove
          chargeMove
          base
  in [makeAttacker quickMove chargeMove |
       quickMove <- quickMoves, chargeMove <- chargeMoves]

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
      -- 14 at level 28 whereas the site and thte hardcoded numbers put
      -- it at level 27.5.
      getCpmForLevel = GameMaster.getCpMultiplier gameMaster
      (cpm, hp) = case raidLevel of
        1 -> (0.61, 600)
        2 -> (0.67, 1800)
        3 -> (0.73, 3000)
        4 -> (0.79, 7500)
        5 -> (0.79, 12500)
        _ -> error "Raid level must be 1, 2, 3, 4, or 5"
      makeStat baseFunc = (fromIntegral $ baseFunc base + 15) * cpm
      makePokemon quickMove chargeMove =
        Pokemon.new
          (PokemonBase.species base)
          (PokemonBase.species base)
          0   -- level, not used
          (PokemonBase.types base)
          (makeStat PokemonBase.attack)
          (makeStat PokemonBase.defense)
          -- Defender's hp is doubled, but that doesn't seem to be
          -- the case for raid bosses, so here we halve the stamins to
          -- compensate for the doubling.
          (hp / 2)
          quickMove
          chargeMove
          base
  in [makePokemon quickMove chargeMove |
       quickMove <- quickMoves, chargeMove <- chargeMoves]

setLevel :: Float -> Battler -> Battler
setLevel level battler =
  case BattlerUtil.level battler of
    Level ivs ->
      battler { level = Level $ IVs.setLevel ivs level }
    _ -> error $ "Can't set level of " ++ show battler
