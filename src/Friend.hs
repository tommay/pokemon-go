module Friend (
  Friend,
  optFriend,
  damageBonus,
) where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>))
import           Data.Semigroup ((<>))

data Friend = Ultra | Best
  deriving (Eq, Show)

optFriend :: O.Parser Friend
optFriend =
      O.flag' Ultra (O.long "ultra" <> O.help "ultra friend")
  <|> O.flag' Best (O.long "best" <> O.help "best friend")

-- Damage bonus from research at
-- https://www.pokebattler.com/articles/2019/06/29/new-higher-friendship-bonus-research-results/
-- 6/2019 after bonus increase
--
damageBonus :: Maybe Friend -> Float
damageBonus maybeFriend =
  case maybeFriend of
    Just Best -> 1.15
    Just Ultra -> 1.10
    _ -> 1.0
--    Just Great -> 1.08    -- Hypothesis
--    Just Good -> 1.05     -- Hypothesis
