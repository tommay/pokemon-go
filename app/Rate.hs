{-# LANGUAGE OverloadedStrings #-}

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
import           System.FilePath ((<.>))
import qualified Text.Printf as Printf
import qualified Text.Regex as Regex

import qualified Debug

data Options = Options {
  filePath :: FilePath,
  league :: League,
  maybeEvolutionSpeciesOrNumber :: Maybe SpeciesOrNumber
}

data SpeciesOrNumber = Species String | Number Int
  deriving (Show)

data ParsedRateLine = ParsedRateLine {
  species      :: String,
  cp           :: Int,
  attack       :: Int,
  defense      :: Int,
  stamina      :: Int
} deriving (Show)

data League = Little | Great | Ultra | Master | Peewee
  deriving (Eq)

instance Show League where
  show this = case this of
    Little -> "little"
    Great -> "great"
    Ultra -> "ultra"
    Master -> "master"

-- XXX This is supposed to be in Data.List
(!?) :: [a] -> Int -> Maybe a
(!?) lst idx =
  case lst of
    [] -> Nothing
    (a:as) ->
      if idx == 0
        then Just a
        else as !? (idx - 1)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optFilePath <*> optLeague <*>
        optMaybeEvolutionSpeciesOrNumber
      optFilePath = O.strArgument
        (O.metavar "FILEPATH"
        <> O.help "file with pokemon to rate")
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
      optMaybeEvolutionSpeciesOrNumber = O.optional $
        let optEvolutionSpecies = Species <$> O.strOption
              (  O.long "evolution"
              <> O.short 'e'
              <> O.metavar "EVOLUTION"
              <> O.help "Evolution for PVP")
            optEvolutionNumber = Number <$> (
                    O.flag' 1 (
                    O.short '1' <>
                    O.help "first evolution")
              <|> O.flag' 2 (
                    O.short '2' <>
                    O.help "second evolution")
              <|> O.flag' 3 (
                    O.short '3' <>
                    O.help "third evolution"))
        in optEvolutionSpecies <|> optEvolutionNumber
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Evaluate pokemon for PVP")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

testXx :: [Int] -> [Int]
testXx lst = do
  n <- lst
  return $ n + 1

getSpeciesToEvolveTo :: Epic.MonadCatch m => GameMaster -> League -> String ->
  Maybe SpeciesOrNumber -> m String
getSpeciesToEvolveTo gameMaster league species maybeEvolvedSpeciesOrNumber = do
  evolutionChains' <- PokeUtil.evolutionChains gameMaster False (species, 0)
  -- Get rid of each evolution's candy and just keep the species name.
  let evolutionChains = map (map fst) evolutionChains'
  case maybeEvolvedSpeciesOrNumber of
    Nothing ->
      case league of
        Little -> return species
        -- No explicit evolution was specified and it's not Little
        -- league so we want the final evolution.  Technically, an
        -- evolutionChain may be an empty list and so not have a
        -- last element, but in practice that can't happen because there
        -- is always at least one evolution: the original species.
        _ -> case map last evolutionChains of
               [evolvedSpecies] -> return evolvedSpecies
               allEvolutions -> Epic.fail $
                 species ++ " has multiple evolutions, choose one of " ++
                   Util.commaSeparated "or" allEvolutions
    Just (Species species) -> return species
    Just (Number number) -> do
      -- Suppose we have the evolutionChains for poliwag.  There are
      -- two: [poliwag, poliwhirl, poliwrath] and [poliwag, poliwhirl,
      -- politoed].  We want to be able to slice this into all the first
      -- evolutions, the second evolutions, and third evolutions:
      -- [poliwag, poliwag], [poliwhirl, poliwhirl], and [poliwrath,
      -- politoed].  That's what transpose does.  Then we use nub to get
      -- the unique evolutions for each stage.
      let evolutionStages = List.transpose evolutionChains
      case evolutionStages !? (number - 1) of
        Nothing -> Epic.fail $
          Printf.printf "%s does not have %d evolutions" species number
        Just evolutionsAtStage ->
          let evolutions = List.nub evolutionsAtStage
          in case evolutions of
            [] -> Epic.fail $
              Printf.printf "%s has no evolutions?" species
            [evolvedSpecies] -> return evolvedSpecies
            _ -> Epic.fail $
              Printf.printf "%s has multiple evolutions: %s" species
                (Util.toLower $ Util.commaSeparated "and" evolutions)

main =
  Epic.catch ( do
    options <- getOptions
    gameMaster <- join $ GameMaster.load

    let filepath = filePath options
        defaultSpecies = FilePath.takeBaseName filepath
        team = FilePath.takeExtension filepath

--    (defaultSpecies, team) <- Epic.toEpic $ parseSpeciesAndTeam filepath

    speciesToEvolveTo <- getSpeciesToEvolveTo gameMaster (league options)
      defaultSpecies (maybeEvolutionSpeciesOrNumber options)

    let outputFileName = FilePath.replaceBaseName filepath speciesToEvolveTo
          <.> show league

    content <- lines <$> readFile filepath

{-
    output :: [Either String Rated]
    output = map (evolveAndRate league speciesToEvolveTo) content

    let uncommented = map removeComment content
        noBlank = filter (/= "") uncommented
-}
{-
    do
      line <- uncommented
      if line == ""
        then []
        else []
-}
        
{-
    parsedRateLine <- mapM (parseRateLine defaultSpecies) noBlank
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
        team <- Text.unpack <$> Atto.takeText
        return $ (species, team)
  in case Atto.parseOnly attoParseSpeciesAndTeam (Text.pack filename) of
    Left _ ->
      Left $ "filename should look like SPECIES.TEAM"
    Right result -> Right $ result

parseRateLine :: String -> String -> Either String ParsedRateLine
parseRateLine defaultSpecies line =
  let attoParseRateLine = do
        species <- Text.unpack <$> (Atto.takeWhile $ Atto.inClass "a-z_")
        let species = if species /= ""
              then species
              else defaultSpecies
        cp <- Atto.decimal
        Atto.skipSpace
        attack <- Atto.decimal
        Atto.skipSpace
        defense <- Atto.decimal
        Atto.skipSpace
        stamina <- Atto.decimal
        Atto.skipSpace
        Atto.endOfInput
        return $ ParsedRateLine {
          species = species,
          cp = cp,
          attack =  attack,
          defense =  defense,
          stamina = stamina
          }
  in case Atto.parseOnly attoParseRateLine (Text.pack line) of
       Left error -> Left $ line ++ ":\n" ++ error
       Right result -> Right $ result

removeComment :: String -> String
removeComment line =
  Regex.subRegex (Regex.mkRegex " *#.*") line ""
