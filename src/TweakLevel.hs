module TweakLevel (
  optTweakLevel
) where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>))
import           Data.Semigroup ((<>))

optTweakLevel :: O.Parser (Float -> Float)
optTweakLevel =
  let optSetLevel = const <$> O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.metavar "LEVEL"
        <> O.help ("Tweak my_pokemon level to find who's implicitly best, " ++
             "or set the level for -a or the default level for -m"))
      optMinLevel = (\l1 l2 -> maximum [l1, l2]) <$> O.option O.auto
        (  O.long "minlevel"
        <> O.short 'n'
        <> O.metavar "LEVEL"
        <> O.help "Tweak level of pokemon read from FILE to be at least LEVEL")
  in optSetLevel <|> optMinLevel <|> pure id
