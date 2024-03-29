{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative (optional, some, many)
import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import qualified Data.Attoparsec.Text as Atto
import           Data.Semigroup ((<>))
import qualified Data.Text as Text

import qualified Calc
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
import qualified Type
import           Type (Type)
import qualified Util

import qualified Debug as D

import           Control.Monad (join, forM, forM_)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

type Battler = (String, (Int, Int, Int))

data League = Little | Great | Ultra | Master | Peewee | FixedCp Int
  deriving (Eq, Show)

data Options = Options {
  attacker :: Battler,
  defender :: Maybe String,
  league :: League,
  classic :: Bool,
  useReturn :: Bool,
  useReturnIfAvailable :: Bool,
  useStatProduct :: Bool
}

getOptions :: GameMaster -> IO Options
getOptions gameMaster =
  let opts = Options <$> optAttacker <*> optDefender <*> optLeague
        <*> optClassic <*> optUseReturn <*> optUseReturnIfAvailable
        <*> optUseStatProduct
      allSpecies = GameMaster.allSpecies gameMaster
      optAttacker = O.argument
        optParseBattler
        (O.metavar "ATTACKER")
      optDefender = O.optional $ O.strArgument
        (  (O.metavar "DEFENDER")
        <> O.completeWith allSpecies)
      optLeague =
            O.flag' Little (
              -- There is no short option because the obvious "-l" conflicts
              -- with the short option for "--level".
              O.long "little" <>
              O.help "little league")
        <|> O.flag' Great (
              O.short 'g' <>
              O.long "great" <>
              O.help "great league")
        <|> O.flag' Ultra (
              O.short 'u' <>
              O.long "ultra" <>
              O.help "ultra league")
        <|> O.flag' Master (
              O.short 'm' <>
              O.long "master" <>
              O.help "master league")
        <|> O.flag' Peewee (
              O.short 'p' <>
              O.long "peewee" <>
              O.help "peewee league")
        <|> FixedCp <$> O.option O.auto
              (  O.long "cp"
              <> O.metavar "N"
              <> O.help "CP is specifies as N")
        <|> pure Great
      optClassic = O.switch (
        O.short 'c' <>
        O.long "classic" <>
        O.help "classsic league (max level 40)")
      optUseReturn = O.switch (
        O.short 'R' <>
        O.help "add the move Return to the movepool")
      optUseReturnIfAvailable = O.switch (
        O.short 'r' <>
        O.long "return" <>
        O.help "add the move Return to the movepool if shadow is available")
      optUseStatProduct = O.switch (
        O.short 's' <>
        O.long "statproduct" <>
        O.help "use statproduct when calculating damage")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Rate movesets by spamminess.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

leaguePred :: League -> (Int -> Bool)
leaguePred league =
  case league of
    Little -> (<= 500)
    Great -> (<= 1500)
    Ultra -> (<= 2500)
    Master -> const True
    Peewee -> (<= 10)
    FixedCp n -> (<= n)

levelPred :: Bool -> (Float -> Bool)
levelPred classic =
  if classic
    then (<= 40)
    else const True

-- XXX this might not be good when higher IVs are better, e.g., master league.
--
defaultIVs = (4, 13, 13)

main = Epic.catch (
    do
      gameMaster <- join $ GameMaster.load
      options <- getOptions gameMaster
      moveReturn <- GameMaster.getCharge gameMaster "return"
      let (attacker, ivs) = Main.attacker options
      base <- GameMaster.getPokemonBase gameMaster $ attacker
      let statProduct = if useStatProduct options
            then getPoweredUpStatProduct gameMaster base
              (leaguePred $ league options) (levelPred $ classic options)
              ivs / 1000000
            else 1
      maybeDefenderBase <- do
        case defender options of
          Just species -> Just <$> GameMaster.getPokemonBase gameMaster species
          Nothing -> return Nothing
      let types = PokemonBase.types base
          defenderTypes = case maybeDefenderBase of
            Just defenderBase -> PokemonBase.types defenderBase
            Nothing -> []
          addReturn =
            (useReturnIfAvailable options &&
              PokemonBase.isShadowAvailable base) ||
            useReturn options
          moveSets = [(fast, charged) |
            fast <- PokemonBase.quickMoves base,
            charged <- PokemonBase.chargeMoves base ++
              if addReturn then [moveReturn] else []]
          multiplier move =
            Move.stabFor move types *
            Move.effectivenessAgainst move defenderTypes
          turns = fromIntegral . (+1) . Move.pvpDurationTurns
          damage move = Move.pvpPower move * multiplier move
          energyGainPerTurn move =
            (fromIntegral $ Move.pvpEnergyDelta move) / turns move
          damagePerTurn move = damage move / turns move * statProduct
          toName :: Move -> Move -> String
          toName fast charged =
            Printf.printf "%s(%.1f/%.1f) / %s(%d/%.0f)"
              (Move.name fast) (energyGainPerTurn fast) (damagePerTurn fast)
              (Move.name charged) (negate $ Move.pvpEnergyDelta charged)
              (damage charged)
          maxNameLength = maximum $
            map (\ (fast, charged) -> length $ toName fast charged) moveSets
          toStuff :: (Move, Move) -> Maybe (String, Int, Int, Float, Float)
          toStuff (fast, charged) = do
            let name = toName fast charged
            (turnsPerCycle, fastMovesPerCycle, dpt, dpe) <-
              maybeSpam types defenderTypes fast charged
            Just $
              (name, turnsPerCycle, fastMovesPerCycle, dpt * statProduct, dpe)
          stuff = Maybe.mapMaybe toStuff moveSets
      forM_ stuff $ \ (name, turnsPerCycle, fastMovesPerCycle, dpt, dpe) ->
        putStrLn $ Printf.printf "%-*s: %2d(%d) %5.2f %.2f"
          maxNameLength name turnsPerCycle fastMovesPerCycle dpt dpe
    )
    $ Exit.die

getPoweredUpStatProduct :: GameMaster -> PokemonBase -> (Int -> Bool) ->
  (Float -> Bool) -> (Int, Int, Int) -> Float
getPoweredUpStatProduct gameMaster base leaguePred levelPred
    (attack, defense, stamina) =
  getStatProduct gameMaster base $
    getPoweredUpIVs gameMaster base leaguePred levelPred
      (attack, defense, stamina)

getPoweredUpIVs :: GameMaster -> PokemonBase -> (Int -> Bool)
  -> (Float -> Bool) -> (Int, Int, Int) -> IVs
getPoweredUpIVs gameMaster base leaguePred levelPred
    (attack, defense, stamina) =
  Util.lastWhere (leaguePred . Calc.cp gameMaster base) $
    map (\ level -> IVs.new level attack defense stamina) $
      filter levelPred $ GameMaster.powerUpLevels gameMaster

getStatProduct :: GameMaster -> PokemonBase -> IVs -> Float
getStatProduct gameMaster base ivs =
  let cpMultiplier = GameMaster.getCpMultiplier gameMaster $ IVs.level ivs
  in foldr (*) 1 $
       map (* cpMultiplier) $
       map fromIntegral $
       map (\ (a, b) -> a base + b ivs)
         [(PokemonBase.attack, IVs.attack),
          (PokemonBase.defense, IVs.defense),
          (PokemonBase.stamina, IVs.stamina)]

-- Sometimes new Pokemon appear in GAME_MASTER with moves that cause
-- dpt to be Infinity.  Check for that and return Nothing instead.
--
maybeSpam :: [Type] -> [Type] -> Move -> Move -> Maybe (Int, Int, Float, Float)
maybeSpam types defenderTypes fast charged = do
  let chargedEnergy = - Move.pvpEnergyDelta charged
      fastEnergy = Move.pvpEnergyDelta fast
  fastMovesPerCycle <- case fastEnergy of
    0 -> Nothing
    _ -> Just $ (chargedEnergy + fastEnergy - 1) `div` fastEnergy
  let turnsPerCycle = fastMovesPerCycle * (Move.pvpDurationTurns fast + 1)
      multiplier move =
        Move.stabFor move types * Move.effectivenessAgainst move defenderTypes
      multiplierFast = multiplier fast
      multiplierCharged = multiplier charged
      damagePerCycle =
        (fromIntegral fastMovesPerCycle) * (Move.pvpPower fast) *
          multiplierFast
        + (Move.pvpPower charged * multiplierCharged)
  dpt <- case turnsPerCycle of
    0 -> Nothing
    _ -> Just $ damagePerCycle / fromIntegral turnsPerCycle
  let dpe = (Move.pvpPower charged) * multiplierCharged /
        fromIntegral chargedEnergy
  Just $ (turnsPerCycle, fastMovesPerCycle, dpt, dpe)

optParseBattler :: O.ReadM Battler
optParseBattler = O.eitherReader parseBattler

parseBattler :: String -> Either String Battler
parseBattler string =
  let attoParseBattler = do
        species <- some $ Atto.notChar ':'
        ivs <- (do
          Atto.char ':'
          attack <- Atto.decimal
          Atto.char '/'
          defense <- Atto.decimal
          Atto.char '/'
          stamina <- Atto.decimal
          return $ (attack, defense, stamina))
          <|> pure defaultIVs
        Atto.endOfInput
        return (species, ivs)
  in case Atto.parseOnly attoParseBattler (Text.pack string) of
    Left _ ->
      Left $ "`" ++ string ++ "' should look like SPECIES[:attack/defense/stamina]"
    Right a -> Right a
