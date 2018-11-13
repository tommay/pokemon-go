-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<**>))
import           Data.Semigroup ((<>))

import qualified Epic
import qualified GameMaster
import qualified PokemonBase
import qualified Type

import           Control.Monad (join, forM_)
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  species :: String
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optSpecies
      optSpecies = O.strArgument (O.metavar "SPECIES")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc
             "Show how effective (or not) move types are against a pokemon")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      base <- GameMaster.getPokemonBase gameMaster $ species options
      let speciesTypes = PokemonBase.types base
      let allTypes = GameMaster.getAllTypes gameMaster
          longest = maximum $ map (length . Type.name) allTypes
          effectiveness =
            zipMap (\typ -> Type.effectivenessAgainst typ speciesTypes)
              allTypes
      forM_ effectiveness $ \ (typ, effectiveness) ->
        Printf.printf "%-*s :%s\n" longest (Type.name typ)
          (display effectiveness)
    )
    $ Exit.die

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap fn list = zip list (map fn list)

display :: Float -> String
display f =
  let d "0.364" = "xxx"
      d "0.510" = " xx"
      d "0.714" = "  x"
      d "1.000" = ""
      d "1.400" = "    o"
      d "1.960" = "    oo"
      d x = x
  in d $ Printf.printf "%.3f" f