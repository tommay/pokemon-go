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

import qualified Debug

import           Control.Monad (join, forM, forM_)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

type Battler = (String, (Int, Int, Int))

data League = Little | Great | Ultra | Master | Peewee
  deriving (Eq, Show)

data Options = Options {
  attacker :: Battler,
  defender :: Maybe String,
  league :: League,
  useStatProduct :: Bool
}

getOptions :: GameMaster -> IO Options
getOptions gameMaster =
  let opts = Options <$> optAttacker <*> optDefender <*> optLeague <*>
        optUseStatProduct
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
        <|> pure Great
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

defaultIVs = (4, 13, 13)

main =
  Epic.catch (
    do
      gameMaster <- join $ GameMaster.load
      options <- getOptions gameMaster
      let (attacker, ivs) = Main.attacker options
      base <- GameMaster.getPokemonBase gameMaster $ attacker
      let statProduct = if useStatProduct options
            then getPoweredUpStatProduct gameMaster base
              (leaguePred $ league options) ivs / 1000000
            else 1
      maybeDefenderBase <- do
        case defender options of
          Just species -> Just <$> GameMaster.getPokemonBase gameMaster species
          Nothing -> return Nothing
      let types = PokemonBase.types base
          defenderTypes = case maybeDefenderBase of
            Just defenderBase -> PokemonBase.types defenderBase
            Nothing -> []
          moveSets = [(fast, charged) |
            fast <- PokemonBase.quickMoves base,
            charged <- PokemonBase.chargeMoves base]
          multiplier move =
            Move.stabFor move types *
            Move.effectivenessAgainst move defenderTypes
          damage move = Move.pvpPower move * multiplier move
          toName :: Move -> Move -> String
          toName fast charged =
            Printf.printf "%s(%d/%.1f) / %s(%d/%.0f)"
              (Move.name fast) (Move.pvpEnergyDelta fast) (damage fast)
              (Move.name charged) (negate $ Move.pvpEnergyDelta charged)
              (damage charged)
          maxNameLength = maximum $
            map (\ (fast, charged) -> length $ toName fast charged) moveSets
          toStuff :: (Move, Move) -> (String, Int, Int, Float, Float)
          toStuff (fast, charged) =
            let name = toName fast charged
                (turnsPerCycle, fastMovesPerCycle, dpt, dpe) =
                  spam types defenderTypes fast charged
            in (name, turnsPerCycle, fastMovesPerCycle, dpt * statProduct, dpe)
          stuff = map toStuff moveSets
      forM_ stuff $ \ (name, turnsPerCycle, fastMovesPerCycle, dpt, dpe) ->
        putStrLn $ Printf.printf "%-*s: %d(%d) %.2f %.2f"
          maxNameLength name turnsPerCycle fastMovesPerCycle dpt dpe
    )
    $ Exit.die

allIvs =
  let ivs = [0..15]
  in [(attack, defense, stamina) |
       attack <- ivs, defense <- ivs, stamina <- ivs]

getPoweredUpStatProduct :: GameMaster -> PokemonBase -> (Int -> Bool) -> (Int, Int, Int) -> Float
getPoweredUpStatProduct gameMaster base leaguePred (attack, defense, stamina) =
  getStatProduct gameMaster base $
    getPoweredUpIVs gameMaster base leaguePred (attack, defense, stamina)

getPoweredUpIVs :: GameMaster -> PokemonBase -> (Int -> Bool) -> (Int, Int, Int) -> IVs
getPoweredUpIVs gameMaster base leaguePred (attack, defense, stamina) =
  Util.lastWhere (leaguePred . Calc.cp gameMaster base) $
    map (\ level -> IVs.new level attack defense stamina) $
    GameMaster.powerUpLevels gameMaster

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

spam :: [Type] -> [Type] -> Move -> Move -> (Int, Int, Float, Float)
spam types defenderTypes fast charged =
  let chargedEnergy = - Move.pvpEnergyDelta charged
      fastEnergy = Move.pvpEnergyDelta fast
      fastMovesPerCycle = (chargedEnergy + fastEnergy - 1) `div` fastEnergy
      turnsPerCycle = fastMovesPerCycle * (Move.pvpDurationTurns fast + 1)
      multiplier move =
        Move.stabFor move types * Move.effectivenessAgainst move defenderTypes
      multiplierFast = multiplier fast
      multiplierCharged = multiplier charged
      damagePerCycle =
        (fromIntegral fastMovesPerCycle) * (Move.pvpPower fast) *
          multiplierFast
        + (Move.pvpPower charged * multiplierCharged)
      dpt = damagePerCycle / fromIntegral turnsPerCycle
      dpe = (Move.pvpPower charged) * multiplierCharged /
        fromIntegral chargedEnergy
  in (turnsPerCycle, fastMovesPerCycle, dpt, dpe)

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
