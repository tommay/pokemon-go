{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Matchup
import           Matchup (Matchup)
import qualified Util

import           Control.Monad (join)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified System.IO as IO
import qualified Text.Printf as Printf

data Options = Options {
  species :: String,
  old :: String,
  new :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optSpecies <*> optOld <*> optNew
      optSpecies = O.argument O.str (O.metavar "SPECIES")
      optOld = O.argument O.str (O.metavar "OLDMOVE")
      optNew = O.argument O.str (O.metavar "NEWMOVE")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Show which matchups get worse if a move is TMd.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"

      matchups <- join $ Matchup.load "matchups.out"

      let worse = getWorseMatchups gameMaster matchups
            (species options) (old options) (new options)

      mapM_ (putStrLn . showWorse) worse
    )
    $ IO.hPutStrLn IO.stderr

showWorse :: (String, String, Int, Int) -> String
showWorse (defender, charge, oldDamage, newDamage) =
  Printf.printf "%s %s: %d => %d" defender charge oldDamage newDamage

getWorseMatchups :: GameMaster -> [Matchup] -> String -> String -> String ->
  [(String, String, Int, Int)]
getWorseMatchups gameMaster matchups species old new =
  let mOld = filter ((== [species, old]) . makeFilterKey) matchups
      mNew = Util.groupBy makeGroupKey $
        filter ((== [species, new]) . makeFilterKey) matchups
  in concat $ map (\ old ->
       case HashMap.lookup (makeGroupKey old) mNew of
         Nothing -> []
         Just news -> 
           Maybe.mapMaybe (\ new ->
             if Matchup.minDamage new < Matchup.minDamage old
               then Just (Matchup.defender old, Matchup.charge old,
                 Matchup.minDamage old, Matchup.minDamage new)
               else Nothing) news)
       mOld

makeFilterKey :: Matchup -> [String]
makeFilterKey =
  sequence [Matchup.attacker, Matchup.quick]

makeGroupKey :: Matchup -> [String]
makeGroupKey =
  sequence [Matchup.defender, Matchup.charge]

