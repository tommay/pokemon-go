module Main where

import           Control.Applicative (optional, some, many)
import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
--import           Data.List ((!?))
import           Data.Semigroup ((<>))

import qualified Calc
import qualified Cost
import           Cost (Cost)
import qualified Discounts
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified IVs
import           IVs (IVs)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil
import qualified Powerups
import qualified Util

import           Control.Monad (join)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

import qualified Debug

data Options = Options {
  filePath :: FilePath
}

data RateLine = RateLine {
  species     :: String,
  cp          :: Int,
  attack      :: Int,
  defense     :: Int,
  stamina     :: Int
} deriving (Show)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilePath
      optFilePath = O.strArgument
        (O.metavar "FILEPATH"
        <> O.help "file with pokemon to rate")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Evaluate pokemon for PVP")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

testXx :: [Int] -> [Int]
testXx lst = do
  n <- lst
  return $ pure $ n + 1

main =
  Epic.catch (do
    options <- getOptions
    let filepath = filePath options
    (species, team) <- case parseSpeciesAndTeam filepath of
      Left error -> Epic.fail error
      Right (species, team) -> return $ (species, team)
    content <- readFile filepath
    let lines' = lines content
        uncommented = map removeComment lines'
        noBlank = filter (/= "") uncommented
    do
      line <- uncommented
      if line == ""
        then []
        else []
        
{-
    rateLine <- mapM (parseRateLine species) noBlank
    let (errors, myPokemon) = Either.partitionEithers $
          map getMyPokemon rateLine
-}

    putStrLn team
  )
  $ Exit.die

parseSpeciesAndTeam :: FilePath -> Either String (String, String)
parseSpeciesAndTeam filepath =
  let filename = FilePath.takeFileName filepath
      attoParseSpeciesAndTeam = do
        species <- some $ Atto.notChar '.'
        Atto.char '.'
        team <- Atto.takeText
        return $ (species, team)
  in case Atto.parseOnly attoParseSpeciesAndTeam (Text.pack filename) of
    Left _ ->
      Left $ "filename should look like SPECIES.TEAM"
    Right result -> Right result

parseRateLine :: String -> String -> Either String RateLine
parseRateLine defaultSpecies line =
  let attoParseRateLine = do
        species <- Atto.takeWhile $ Atto.inClass "a-z_"
        let species' = if species == ""
              then defaultSpecies
              else species
        cp <- Atto.decimal
        Atto.skipSpace
        attack <- Atto.decimal
        Atto.skipSpace
        defense <- Atto.decimal
        Atto.skipSpace
        stamina <- Atto.decimal
        Atto.skipSpace
        Atto.endOfInput
        return $ RateLine species' cp attack defense stamina
  in case Atto.parseOnly attoParseSpeciesAndTeam (Text.pack string) of
       Left error -> Epic.fail $ RateLine ++ ":\n" ++ error
       Right result -> Right result

removeComment :: String -> String
removeComment line =
  Regex.subRegex (Regex.mkRegex " *#.*") line ""
