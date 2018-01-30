module ForceLevel (
  optForceLevel
) where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>))
import           Data.Semigroup ((<>))

optForceLevel :: O.Parser (Float -> Float)
optForceLevel =
  let optSetLevel = const <$> O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.metavar "LEVEL"
        <> O.help ("Force my_pokemon level to find who's implicitly best, " ++
             "or set the level for -a or the default level for -m"))
      optMinLevel = (\l1 l2 -> maximum [l1, l2]) <$> O.option O.auto
        (  O.long "minlevel"
        <> O.short 'n'
        <> O.metavar "LEVEL"
        <> O.help ("Force level of pokemon read from FILE to be LEVEL"))
  in optSetLevel <|> optMinLevel <|> pure id

{-
getLevel :: ForceLevel -> Float -> Float
getLevel forceLevel level =
  case forceLevel of
    SetLevel val -> val
    MinLevel val -> maximum [val, level]
-}

