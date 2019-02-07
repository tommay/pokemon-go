{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import           Battle (Battle)
import qualified BattlerUtil
import           BattlerUtil (Battler, Level (Normal))
import qualified Breakpoint
import qualified Calc
import qualified Epic
import qualified TweakLevel
import qualified IVs
import           IVs (IVs)
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import           Move (Move)
import qualified MakePokemon
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Mythical
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil
import qualified Powerups
import qualified Type
import           Type (Type)
import qualified Weather
import           Weather (Weather (..))

import qualified Debug

import           Control.Monad (join, forM, forM_)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  species :: String,
  cpList :: [Int]
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optSpecies <*> optCps
      optSpecies = O.strArgument (O.metavar "SPECIES")
      optCps = O.some $ O.argument O.auto (O.metavar "CP")
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
      putStrLn $ show $ NestedResults $ for (cpList options) $ \ cp ->
            let ivs = getIvs gameMaster base cp
            in NestedResults $ for evolutionBases $ \ evolutionBase ->
              let evolutionSpecies = PokemonBase.species evolutionBase
                  evolvedCps = map (Calc.cp gameMaster evolutionBase) ivs
                  min = minimum evolvedCps
                  max = maximum evolvedCps
              in NestedResult $ if min == max
                then Printf.printf "%s: %d" evolutionSpecies min
                else Printf.printf "%s: %d - %d" evolutionSpecies min max
  )
  $ Exit.die

for :: [a] -> (a -> b) -> [b]
for = flip map

getEvolutions :: Epic.MonadCatch m => GameMaster -> String -> m [String]
getEvolutions gameMaster species = do
  -- [[(String, Int)]]
  evolutionChains <- PokeUtil.evolutionChains gameMaster (species, 0)
  return $ List.nub $ map fst $ concat evolutionChains

getIvs :: GameMaster -> PokemonBase -> Int -> [IVs]
getIvs gameMaster base cp =
  -- Assume that if a pokemon hasn't been evolved it also hasn't been
  -- powered up so we're only concerned with whole levels.
  let wholeLevels = filter isWholeNumber $ GameMaster.allLevels gameMaster
      allIvs = [IVs.new level attack defense stamina |
                 level <- wholeLevels,
                 let ivs = [0 .. 15],
                 attack <- ivs, defense <- ivs, stamina <- ivs]
  in filter ((== cp) . Calc.cp gameMaster base) allIvs

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