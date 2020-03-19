-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import qualified PokemonBase
import qualified Type
import qualified Util

import           Control.Monad (join, forM_)
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  speciesOrType :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optSpeciesOrType
      optSpeciesOrType = O.strArgument (O.metavar "SPECIES-OR-TYPE")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc
             "Show how effective (or not) move types are against a pokemon or type")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      let speciesOrType = Main.speciesOrType options
      types <- do
        case GameMaster.getType gameMaster speciesOrType of
          Right tjpe -> return $ [tjpe]
          Left _ ->
            case GameMaster.getPokemonBase gameMaster speciesOrType of
              Right base -> return $ PokemonBase.types base
              Left _ ->
                Epic.fail $ "No such species or type: " ++ speciesOrType
      let allTypes = List.sortOn Type.name $ GameMaster.getAllTypes gameMaster
          longest = maximum $ map (length . Type.name) allTypes
          effectiveness =
            Util.augment (\typ -> Type.effectivenessAgainst typ types) allTypes
      Printf.printf "type: %s\n" $ List.intercalate ", " $ map Type.name types
      forM_ effectiveness $ \ (typ, effectiveness) ->
        Printf.printf "%-*s :%s\n" longest (Type.name typ)
          (display effectiveness)
    )
    $ Exit.die

display :: Float -> String
display f =
  let d "0.244" = "xxx"
      d "0.391" = " xx"
      d "0.625" = "  x"
      d "1.000" = ""
      d "1.600" = "    o"
      d "2.560" = "    oo"
      d x = x
  in d $ Printf.printf "%.3f" f
