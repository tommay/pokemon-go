module ForceLevel (
  ForceLevel,
  optForceLevel,
  getLevel,
) where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>))
import           Data.Semigroup ((<>))

data ForceLevel = SetLevel Float | MinLevel Float
  deriving (Show)

optForceLevel :: O.Parser ForceLevel
optForceLevel =
  let optSetLevel = SetLevel <$> O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.metavar "LEVEL"
        <> O.help ("Force my_pokemon level to find who's implicitly best, " ++
             "or set the level for -a or the default level for -m"))
      optMinLevel = MinLevel <$> O.option O.auto
        (  O.long "minlevel"
        <> O.short 'n'
        <> O.metavar "LEVEL"
        <> O.help ("Force level of pokemon read from FILE to be LEVEL"))
  in optSetLevel <|> optMinLevel

getLevel :: ForceLevel -> Float -> Float
getLevel forceLevel level =
  case forceLevel of
    SetLevel val -> val
    MinLevel val -> maximum [val, level]
