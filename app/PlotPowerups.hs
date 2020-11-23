module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Battle
import qualified BattlerUtil
import           BattlerUtil (Battler, Level (Normal))
import qualified Epic
import qualified IVs
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Log
import qualified Logger
import qualified MakePokemon
import qualified Move
import           Move (Move)
import qualified MyPokemon
import           MyPokemon (MyPokemon)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokemonBase
import           PokemonBase (PokemonBase)
import qualified PokeUtil
import           Type (Type)
import qualified Util
import qualified Weather
import           Weather (Weather)
import           WeatherBonus (WeatherBonus)

import qualified Debug

import           Control.Applicative (optional, some, many)
import           Control.Monad (join, mapM, forM, forM_)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

data Options = Options {
  plotCandy :: Bool,
  plotDps :: Bool,
  attackerSource :: AttackerSource,
  maybeEvolution :: Maybe String,
  maybeMoveset   :: Maybe Moveset,
  defender :: Battler,
  maybeWeather :: Maybe Weather  
}

data AttackerSource =
    FromFile FilePath (Maybe Int)
  | MovesetsFor Battler
  deriving (Show)

data Result = Result {
  candy :: Int,
  stardust :: Int,
  dps :: Float,
  tdo :: Int
}

type Moveset = (String, String)

getOptions :: IO Options
getOptions =
  let opts = Options <$> optPlotCandy <*> optPlotDps
        <*> optAttackerSource <*> optEvolution <*> optMoveset
        <*> optDefender <*> optWeather
      optPlotCandy =
        let optPlotCandy =
              O.flag' True
              (  O.long "candy"
              <> O.short 'c'
              <> O.help "Plot candy on the X axis")
            optPlotStardust =
              O.flag' False
              (  O.long "stardust"
              <> O.short 's'
              <> O.help "Plot stardust on the X axis")
         in optPlotCandy <|> optPlotStardust <|> pure True
      optPlotDps =
        let optPlotDps =
              O.flag' True
              (  O.long "dps"
              <> O.short 'd'
              <> O.help "Plot dps on the Y axis")
            optPlotTdo =
              O.flag' False
              (  O.long "tdo"
              <> O.short 't'
              <> O.help "Plot tdo on the Y axis")
         in optPlotDps <|> optPlotTdo <|> pure True
      optAttackerSource =
        let optFilename = FromFile <$>
              O.strOption
              (  O.long "file"
              <> O.short 'f'
              <> O.metavar "FILE"
              <> O.help "File to read my_pokemon from") <*> optMaybeN
            optMaybeN = O.optional $ O.option O.auto
              (  O.long "n"
              <> O.short 'n'
              <> O.metavar "N"
              <> O.help "Only plot the first N pokemon")
            optMovesetsFor = MovesetsFor <$>
              (O.option BattlerUtil.optParseBattler)
              (  O.long "movesets"
              <> O.short 'M'
              <> O.metavar "ATTACKER[:LEVEL]"
              <> O.help "Plot the movesets for ATTACKER against DEFENDER")
        in optFilename <|> optMovesetsFor
      optEvolution = O.optional $ O.strOption
        (  O.long "evolution"
        <> O.short 'e'
        <> O.metavar "EVOLUTION"
        <> O.help "Final evolution desired")
      optMoveset = O.optional $ O.option optParseMoveset
        (  O.long "moveset"
        <> O.short 'm'
        <> O.metavar "ATTACKER_MOVESET"
        <> O.help "Override the attacker moveset")
      optDefender = O.argument
        BattlerUtil.optParseBattler
        (O.metavar "DEFENDER[:LEVEL]")
      optWeather = O.optional Weather.optWeather
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc ("Create gnuplot data for plotting dps/tdo" ++
             " vs. powerup candy"))
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

optParseMoveset :: O.ReadM Moveset
optParseMoveset = O.eitherReader parseMoveset

parseMoveset :: String -> Either String Moveset
parseMoveset string =
  let attoParseMoveset = do
        quickName <- many $ Atto.notChar '/'
        Atto.char '/'
        chargeName <- many $ Atto.anyChar
        Atto.endOfInput
        return (quickName, chargeName)
  in case Atto.parseOnly attoParseMoveset (Text.pack string) of
    Left _ -> Left $ "`" ++ string ++ "' should look like FAST/CHARGE"
    Right moveset -> Right moveset

main =
  Epic.catch (
    do
      options <- getOptions

      gameMaster <- join $ GameMaster.load

      let weatherBonus =
            GameMaster.getWeatherBonus gameMaster $ maybeWeather options

      defenderVariants <-
        BattlerUtil.makeBattlerVariants gameMaster $ defender options

      myPokemon <- case attackerSource options of
        FromFile filename maybeN -> do
          -- myPokemon is a [MyPokemon] with one MyPokemon for
          -- each entry in the file.  Each MyPokemon may have
          -- multiple possible IV sets.
          myPokemon <- join $ MyPokemon.load $ Just filename
          case maybeN of
            Just n -> return $ take n myPokemon
            Nothing -> return $ myPokemon
        MovesetsFor attacker -> do
          attackerVariants <-
            BattlerUtil.makeBattlerVariants gameMaster attacker
          return $ map makeMyPokemonFromPokemon attackerVariants

      -- Evolve all pokemon to their highest level and remember how
      -- much candy it took.

      myPokemonAndCandy <- do
        let maybeEvolution = Main.maybeEvolution options
        mapM (PokeUtil.evolveFullyWithCandy gameMaster maybeEvolution) myPokemon

      myPokemonAndCandy <- case maybeMoveset options of
        Nothing -> return myPokemonAndCandy
        Just (fastName, chargeName) ->
          forM myPokemonAndCandy $ \ (pokemon, candy) -> do
            pokemon <- setMoves gameMaster fastName chargeName pokemon
            return (pokemon, candy)

      let commands = reverse $ fst $
            foldl (\ (commands, (lastColor, lastDash)) myPokemon ->
              let color = if List.notElem '[' $ MyPokemon.name myPokemon
                    then lastColor + 1
                    else lastColor
                  thisDash = if color /= lastColor
                    then 0
                    else lastDash + 1
                  dashType = if thisDash == 0
                    then "1"
                    else case (thisDash - 1) `mod` 3 of
                      0 -> "3"
                      1 -> "5"
                  pointType = if color < numColors then 12 else 2 :: Int
                  command = Printf.printf ("  \"-\" using 1:2" ++
                    " with linespoints lw 2 pt %d lc %s dt %s title \"%s\"")
                    pointType (getColor color) dashType
                    (MyPokemon.name myPokemon)
              in (command : commands, (color, thisDash)))
              ([], (-1, 0))
              (map fst myPokemonAndCandy)

      putStrLn "plot \\"
      putStrLn $ List.intercalate ", \\\n" commands

      forM_ myPokemonAndCandy $ \ (myPokemon, candy) -> do
        results <- getResultsForAllPowerups gameMaster weatherBonus candy
          myPokemon defenderVariants
        forM_ results $ \ result -> do
          Printf.printf "%d %f\n"
            (if plotCandy options
              then Main.candy result
              else Main.stardust result)
            (if plotDps options
              then Main.dps result
              else fromIntegral $ Main.tdo result)
        putStrLn "e"
    )
    $ Exit.die

-- "Dumb down" a Pokemon into a MyPokemon.  The one advantage of using
-- MyPokemon instead of using Pokemon throughout is that when we
-- evolve and do MyPokemon.setSpecies the moves will be checked for
-- compatibility in MakePokemon.makePokemon.
--
makeMyPokemonFromPokemon :: Pokemon -> MyPokemon
makeMyPokemonFromPokemon pokemon =
  MyPokemon.MyPokemon {
    MyPokemon.name =
      Printf.printf "%s %s/%s %d"
      (Pokemon.pname pokemon)
      (Move.name $ Pokemon.quick pokemon)
      (Move.name $ Pokemon.charge pokemon)
      (Move.bars $ Pokemon.charge pokemon),
    MyPokemon.species = PokemonBase.species $ Pokemon.base pokemon,
    MyPokemon.cp = undefined,
    MyPokemon.hp = undefined,
    MyPokemon.stardust = undefined,
    MyPokemon.quickName = Move.name $ Pokemon.quick pokemon,
    MyPokemon.chargeName = Move.name $ Pokemon.charge pokemon,
    MyPokemon.maybeChargeName2 = Nothing,
    MyPokemon.ivs = Pokemon.ivs pokemon,
    MyPokemon.stats = undefined
  }

setMoves :: Epic.MonadCatch m =>
  GameMaster -> String -> String -> MyPokemon -> m MyPokemon
setMoves gameMaster quickName chargeName myPokemon = do
  base <-
    GameMaster.getPokemonBase gameMaster $ MyPokemon.species myPokemon
  quickName <- getMoveName "quick" PokemonBase.quickMoves quickName base
  chargeName <- getMoveName "charge" PokemonBase.chargeMoves chargeName base
  return $
    MyPokemon.setQuickName quickName $
    MyPokemon.setChargeName chargeName myPokemon

getMoveName :: Epic.MonadCatch m => String -> (PokemonBase -> [Move]) -> String -> PokemonBase -> m String
getMoveName moveType getMovesFunc moveAbbrev base = do
  let moves = getMovesFunc base
      species = PokemonBase.species base
  move <- BattlerUtil.getMatchingMove moveAbbrev moves moveType species
  return $ Move.name move

getResultsForAllPowerups :: Epic.MonadCatch m =>
  GameMaster -> WeatherBonus -> Int -> MyPokemon -> [Pokemon] -> m [Result]
getResultsForAllPowerups gameMaster weatherBonus candy myPokemon defenderVariants = do
  allPowerups <- allPowerups gameMaster candy myPokemon
  mapM (getResult gameMaster weatherBonus defenderVariants) allPowerups

getResult :: Epic.MonadCatch m =>
  GameMaster -> WeatherBonus -> [Pokemon] -> (Int, Int, MyPokemon) -> m Result
getResult gameMaster weatherBonus defenderVariants (candy, stardust, myPokemon) = do
  attackerVariants <- MakePokemon.makePokemon gameMaster myPokemon
  let (dps, tdo) = getMinDpsTdo weatherBonus attackerVariants defenderVariants
  return Result {
    candy = candy,
    stardust = stardust,
    dps = dps,
    tdo = tdo
  }

getMinDpsTdo :: WeatherBonus -> [Pokemon] -> [Pokemon] -> (Float, Int)
getMinDpsTdo weatherBonus attackerVariants defenderVariants =
  let makeBattle attacker defender =
        Battle.init attacker defender `Battle.setWeatherBonus` weatherBonus
      battleLoggers =
        [Battle.doBattle $ makeBattle attacker defender |
          attacker <- attackerVariants, defender <- defenderVariants]
      -- battleResults :: [[Log Battle]], where each [Battle] is the
      -- move-by-move starting from the initial Battle state.
      battleResults = map (fst . Logger.runLogger) battleLoggers
      dps = minimum $ map Battle.dps battleResults
      tdo = minimum $ map Battle.damageInflicted battleResults
  in (dps, tdo)

-- This set of functions to take a Pokemon and return a list of all
-- its powerups along with the required candy and stardust looks a
-- ittle overkill but it was done to build things up in small
-- potentially reusable steps instead of going it in one function.

allPowerups :: Epic.MonadCatch m =>
  GameMaster -> Int -> MyPokemon -> m [(Int, Int, MyPokemon)]
allPowerups gameMaster candy myPokemon =
  iterateWhileJust (cumulativePowerup gameMaster) (candy, 0, myPokemon)

-- This is like iterate but stops when the function returns Nothing.
-- Perhaps this could be done with something from Control.Monad.Loops,
-- but I don't understand a lot of that stuff.
--
iterateWhileJust :: Epic.MonadCatch m => (a -> m (Maybe a)) -> a -> m [a]
iterateWhileJust func a = do
  let iterateWhileJust' a = do
        case a of
          Nothing -> return []
          Just a -> do
            b <- func a
            as <- iterateWhileJust' b
            return $ a : as
  iterateWhileJust' $ Just a

-- Given a MyPokemon and the cumulative candy and stardust it took to
-- power it to its current level, power it up once more and return it
-- with the updated candy and dust costs.
--
cumulativePowerup :: Epic.MonadCatch m =>
  GameMaster -> (Int, Int, MyPokemon) -> m (Maybe (Int, Int, MyPokemon))
cumulativePowerup gameMaster (candy, stardust, myPokemon) = do
  powerup <- powerup gameMaster myPokemon
  return $ case powerup of
    Just (candy', stardust', myPokemon') ->
      Just (candy + candy', stardust + stardust', myPokemon')
    Nothing -> Nothing

-- Here we do thw actual powerup.  Returns Just (candy, stardust,
-- poweredUpMyPokemon) if the MyPokemon is not yet at max level, else
-- Nothing.
--
powerup :: Epic.MonadCatch m =>
  GameMaster -> MyPokemon -> m (Maybe (Int, Int, MyPokemon))
powerup gameMaster myPokemon = do
  let level = MyPokemon.level myPokemon
  return $ case GameMaster.nextLevel gameMaster level of
    Just (candy, dust, level) ->
      Just (candy, dust, MyPokemon.setLevel myPokemon level)
    Nothing -> Nothing

colors = map (Printf.printf "rgb \"%s\"")
  [
  "#ff0000",
  "#00cc00",
  "#0000dd",
  "#ff00ff",
  "#00acf7",
  "#f38100",
  "#d4d400",
  "#009000",
  "#707070"
  ]

numColors = length colors

getColor :: Int -> String
getColor n =
  colors !! (n `mod` numColors)
