-- So .: works with literal Strings.
{-# LANGUAGE OverloadedStrings #-}

module GameMaster (
  GameMaster,
  load,
  getPokemonBase,
  getQuick,
  getCharge,
  getCpMultiplier,
  getLevelsForStardust
) where

import           Data.Text.Conversions (convertText)
import           Data.Char (toLower, toUpper)
import           Data.Hashable (Hashable)
import           Data.Maybe (mapMaybe)
import qualified Data.Yaml as Yaml
import           Data.Yaml (ParseException)
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector
import           Data.Vector (Vector, (!))
import qualified Text.Regex as Regex

import qualified Epic
import           StringMap (StringMap)
import qualified Type
import           Type (Type)
import qualified Move
import           Move (Move)
import qualified PokemonBase
import           PokemonBase (PokemonBase)

data GameMaster = GameMaster {
  pokemonBases  :: StringMap PokemonBase,
  moves         :: StringMap Move,
  cpMultipliers :: Vector Float,
  stardustCost  :: [Integer]
} deriving (Show)

type ItemTemplate = Yaml.Object

load :: Epic.MonadCatch m => FilePath -> IO (m GameMaster)
load filename = do
  either <- Yaml.decodeFileEither filename
  case either of
    Left yamlParseException -> Epic.fail $ show yamlParseException
    Right yamlObject -> return $ makeGameMaster yamlObject

getPokemonBase :: Epic.MonadCatch m => GameMaster -> String -> m PokemonBase
getPokemonBase this speciesName =
  GameMaster.lookup "species" (pokemonBases this) speciesName

getMove :: Epic.MonadCatch m => GameMaster -> String -> m Move
getMove this moveName  =
  GameMaster.lookup "move" (moves this) moveName

getQuick :: Epic.MonadCatch m => GameMaster -> String -> m Move
getQuick this moveName = do
  move <- getMove this (moveName ++ "_fast")
  case Move.isQuick move of
    True -> return move
    False -> Epic.fail $ moveName ++ " is not a quick move"

getCharge :: Epic.MonadCatch m => GameMaster -> String -> m Move
getCharge this moveName = do
  move <- getMove this moveName
  case Move.isCharge move of
    True -> return move
    False -> Epic.fail $ moveName ++ " is not a charge move"

getCpMultiplier :: GameMaster -> Float -> Float
getCpMultiplier this level =
  let cpMultipliers' = GameMaster.cpMultipliers this
      intLevel = floor level
  in case fromIntegral intLevel == level of
    True -> cpMultipliers' ! (intLevel - 1)
    False ->
      let cp0 = cpMultipliers' ! (intLevel - 1)
          cp1 = cpMultipliers' ! intLevel
      in sqrt $ (cp0*cp0 + cp1*cp1) / 2

{-
getCpMultiplier :: Epic.MonadCatch m => GameMaster -> Float -> m Float
getCpMultiplier this level =
  let cpMutipliers = GameMaster.cpMultiplers this
      floorLevel = floor level
  in case floorLevel == level of
    True -> case cpMultiploers !? floorLevel of
      Nothing -> Epic.fail $ "bad level: " ++ (show level)
      Just cpMultiploer -> return cpMultiplier
    False ->
-}

getLevelsForStardust :: (Epic.MonadCatch m) => GameMaster -> Integer -> m [Float]
getLevelsForStardust this starDust = do
  let levels =  concat $ map (\ (n, dust) ->
        if dust == starDust
          then [n, n + 0.5]
          else [])
        $ zip [1..] $ stardustCost this
  case levels of
    [] -> Epic.fail $ "Bad dust amount: " ++ show starDust
    _ -> return levels

lookup :: Epic.MonadCatch m => String -> StringMap a -> String -> m a
lookup what hash key =
  case HashMap.lookup (sanitize key) hash of
    Just val -> return val
    Nothing -> Epic.fail $ "No such " ++ what ++ ": " ++ key

sanitize :: String -> String
sanitize string =
  let nonWordChars = Regex.mkRegex "\\W"
  in map toUpper $ Regex.subRegex nonWordChars string "_"

makeGameMaster :: Epic.MonadCatch m => Yaml.Object -> m GameMaster
makeGameMaster yamlObject = do
  itemTemplates <- getItemTemplates yamlObject
  types <- getTypes itemTemplates
  moves <- getMoves types itemTemplates
  pokemonBases <-
    makeObjects "pokemonSettings" "pokemonId"
      (makePokemonBase types moves) itemTemplates
  cpMultipliers <- do
    playerLevel <- getFirst itemTemplates "playerLevel"
    getObjectValue playerLevel "cpMultiplier"
  stardustCost <- do
    pokemonUpgrades <- getFirst itemTemplates "pokemonUpgrades"
    getObjectValue pokemonUpgrades "stardustCost"
  return $ GameMaster pokemonBases moves cpMultipliers stardustCost

-- Here it's nice to use Yaml.Parser because it will error if we don't
-- get a [ItemTemplate], i.e., it checks that the Yaml.Values are the
-- correct type thanks to type inference.
--
getItemTemplates :: Epic.MonadCatch m => Yaml.Object -> m [ItemTemplate]
getItemTemplates yamlObject =
  toEpic $ Yaml.parseEither (.: "itemTemplates") yamlObject

getTypes :: Epic.MonadCatch m => [ItemTemplate] -> m (StringMap Type)
getTypes itemTemplates = do
  battleSettings <- getFirst itemTemplates "battleSettings"
  stab <- getObjectValue battleSettings "sameTypeAttackBonusMultiplier"
  makeObjects "typeEffective" "attackType" (makeType stab) itemTemplates

makeType :: Epic.MonadCatch m => Float -> ItemTemplate -> m Type
makeType stab itemTemplate = do
  attackScalar <- getObjectValue itemTemplate "attackScalar"
  let effectiveness = HashMap.fromList $ zip effectivenessOrder attackScalar
  name <- getObjectValue itemTemplate "attackType"
  return $ Type.Type name effectiveness stab

effectivenessOrder :: [String]
effectivenessOrder =
  map (\ ptype -> "POKEMON_TYPE_" ++ map toUpper ptype)
    ["normal",
     "fighting",
     "flying",
     "poison",
     "ground",
     "rock",
     "bug",
     "ghost",
     "steel",
     "fire",
     "water",
     "grass",
     "electric",
     "psychic",
     "ice",
     "dragon",
     "dark",
     "fairy"]

getMoves :: Epic.MonadCatch m => StringMap Type -> [ItemTemplate] -> m (StringMap Move)
getMoves types itemTemplates =
  makeObjects "moveSettings" "movementId" (makeMove types) itemTemplates

makeMove :: Epic.MonadCatch m => StringMap Type -> ItemTemplate -> m Move
makeMove types itemTemplate = do
  let getTemplateValue text = getObjectValue itemTemplate text
  Move.Move
    <$> getTemplateValue "movementId"
    <*> do
      typeName <- getTemplateValue "pokemonType"
      get types typeName
    <*> getObjectValueWithDefault itemTemplate "power" 0
    <*> ((/1000) <$> getTemplateValue "durationMs")
    <*> getObjectValueWithDefault itemTemplate "energyDelta" 0

makePokemonBase :: Epic.MonadCatch m => StringMap Type -> StringMap Move -> ItemTemplate -> m PokemonBase
makePokemonBase types moves pokemonSettings =
  Epic.catch (do
    let getValue key = getObjectValue pokemonSettings key

    species <- do
      species <- getValue "pokemonId"
      return $ map toLower species

    ptypes <- do
      ptype <- getValue "type"
      let ptypes = case getValue "type2" of
            Right ptype2 -> [ptype, ptype2]
            -- XXX This can swallow parse errors?
            Left _ -> [ptype]
      mapM (get types) ptypes
  
    statsObject <- getValue "stats"
    attack <- getObjectValue statsObject "baseAttack"
    defense <- getObjectValue statsObject "baseDefense"
    stamina <- getObjectValue statsObject "baseStamina"

    evolutions <- do
      case getValue "evolutionBranch" of
        Right evolutionBranch ->
          mapM (\ branch -> getObjectValue branch "evolution") evolutionBranch
        -- XXX This can swallow parse errors?
        Left _ -> return []

    let getMoves key = do
          moveNames <- getValue key
          mapM (get moves) moveNames

    quickMoves <- getMoves "quickMoves"
    chargeMoves <- getMoves "cinematicMoves"

    let parent = case getValue "parentPokemonId" of
          Right parent -> Just parent
          -- XXX This can swallow parse errors?
          Left _ -> Nothing

    return $ PokemonBase.PokemonBase species ptypes attack defense stamina
       evolutions quickMoves chargeMoves parent)
  (\ex -> Epic.fail $ ex ++ " in " ++ (show pokemonSettings))

makeObjects :: Epic.MonadCatch m => String -> String -> (ItemTemplate -> m a) -> [ItemTemplate]
  -> m (StringMap a)
makeObjects filterKey nameKey makeObject itemTemplates =
  foldr (\ itemTemplate maybeHash -> do
      hash <- maybeHash
      name <- getObjectValue itemTemplate nameKey
      obj <- makeObject itemTemplate
      return $ HashMap.insert name obj hash)
    (pure HashMap.empty)
    $ getAll itemTemplates filterKey

getAll :: [ItemTemplate] -> String -> [ItemTemplate]
getAll itemTemplates filterKey =
  mapMaybe (\ itemTemplate ->
    case getObjectValue itemTemplate filterKey of
      Right value -> Just value
      _ -> Nothing)
    itemTemplates

getFirst :: Epic.MonadCatch m => [ItemTemplate] -> String -> m ItemTemplate
getFirst itemTemplates filterKey =
  case getAll itemTemplates filterKey of
    [head] -> return head
    _ -> Epic.fail $ "Expected exactly one " ++ show filterKey

-- This seems roundabout, but the good thing is that the type "a" is
-- inferred from the usage context so the result is error-checked.
--
getObjectValue :: Epic.MonadCatch m => FromJSON a => Yaml.Object -> String -> m a
getObjectValue yamlObject key =
  toEpic $ Yaml.parseEither (.: convertText key) yamlObject

getObjectValueWithDefault :: Epic.MonadCatch m => FromJSON a => Yaml.Object -> String -> a -> m a
getObjectValueWithDefault yamlObject key dflt =
  toEpic $ Yaml.parseEither (\p -> p .:? convertText key .!= dflt) yamlObject

get :: Epic.MonadCatch m => StringMap a -> String -> m a
get map key =
  case HashMap.lookup key map of
    Just value -> return value
    _ -> Epic.fail $ "Key not found: " ++ show key

toEpic :: (Show a, Epic.MonadCatch m) => Either a b -> m b
toEpic either =
  case either of
    Left err -> Epic.fail (show err)
    Right val -> return val
