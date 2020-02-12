{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Ratio as Ratio
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Epic
import qualified IVs
import           IVs (IVs)
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil

import qualified Debug

import           Control.Monad (join)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  level :: Maybe Float,
  ivFloor :: Maybe Int,
  species :: String,
  cpList :: [Int]   -- Possibly empty.
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLevel <*> optIvFloor <*> optSpecies <*> optCpList
      optLevel = O.optional $ O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.metavar "LEVEL"
        <> O.help "Set pokemon level, e.g., for a raid boss or egg hatch")
      optIvFloor = O.optional $ O.option O.auto
        (  O.long "ivFloor"
        <> O.short 'm'
        <> O.metavar "MINIMUM"
        <> O.help "Set minimum IV, e.g., 10 for raid boss or hatch")
      optSpecies = O.strArgument (O.metavar "SPECIES")
      optCpList = O.many $ O.argument O.auto (O.metavar "CP")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Show evolved CP")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      base <- GameMaster.getPokemonBase gameMaster $ species options
      evolutions <- getEvolutions gameMaster $ PokemonBase.species base
      evolutionBases <- mapM (GameMaster.getPokemonBase gameMaster) evolutions
      let testIVs = getIVsToTest gameMaster (level options) (ivFloor options)
      let resultsForIvs ivList =
            NestedResults $ for evolutionBases $ \ evolutionBase ->
              let evolutionSpecies = PokemonBase.species evolutionBase
                  evolvedCps = map (Calc.cp gameMaster evolutionBase) ivList
                  min = minimum evolvedCps
                  max = maximum evolvedCps
                  cpString :: String
                  cpString = if min == max
                    then Printf.printf "%s: %d" evolutionSpecies min
                    else Printf.printf "%s: %d - %d" evolutionSpecies min max
                  showAsPercent :: Ratio Int -> String
                  showAsPercent n =
                    let percent = n * 100
                    in if Ratio.denominator percent == 1
                         then show $ Ratio.numerator percent
                         else Printf.printf (if percent < 10
                           then "%.1f" else "%.0f")
                           $ (realToFrac percent :: Float)
                  percentLeague cpCap = showAsPercent $
                    (length $ filter (<= cpCap) evolvedCps) %
                    (length evolvedCps)
                  result = Printf.printf "%s (%s%%, %s%%)" cpString
                    (percentLeague 1500) (percentLeague 2500)
              in NestedResult $ result
      putStrLn $ show $ case cpList options of
        [] ->
          resultsForIvs testIVs
        cps ->
          NestedResults $ for cps $ \ cp ->
            resultsForIvs $ filter ((== cp) . Calc.cp gameMaster base) testIVs
  )
  $ Exit.die

for :: [a] -> (a -> b) -> [b]
for = flip map

getEvolutions :: Epic.MonadCatch m => GameMaster -> String -> m [String]
getEvolutions gameMaster species = do
  -- [[(String, Int)]]
  evolutionChains <- PokeUtil.evolutionChains gameMaster (species, 0)
  return $ List.nub $ map fst $ concat evolutionChains

getIVsToTest :: GameMaster -> Maybe Float -> Maybe Int -> [IVs]
getIVsToTest gameMaster maybeLevel maybeIvFloor =
  -- Assume that if a pokemon hasn't been evolved it also hasn't been
  -- powered up so we're only concerned with whole levels.
  let wholeLevels = filter isWholeNumber $ GameMaster.allLevels gameMaster
      levels = case maybeLevel of
        Nothing -> wholeLevels
        Just level -> [level]
      ivs = [Maybe.fromMaybe 0 maybeIvFloor .. 15]
  in [IVs.new level attack defense stamina |
       level <- levels,
       attack <- ivs, defense <- ivs, stamina <- ivs]

isWholeNumber :: Float -> Bool
isWholeNumber n =
  (fromIntegral $ floor n) == n

-- NestedResult is supposed to make the code prettier by allowing
-- arbitrarily nested Lists of result Strings to be printed with the
-- approproate number of newlines between each (sub)set of results.
-- This code itself looks a little wonky though.

data NestedResult = NestedResult String | NestedResults [NestedResult]
instance Show NestedResult where
  show result =
    let show' result =
          case result of
            NestedResult string -> (string, 1)
            NestedResults list ->
              let pieces = map show' list
                  strings = map fst pieces
                  depth = maximum $ map snd pieces
                  newlines = concat $ replicate depth "\n"
              in (List.intercalate newlines strings, depth + 1)
    in fst $ show' result
