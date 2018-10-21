module Breakpoint (
   getBreakpoints,
) where

import qualified Battle
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokeUtil
import           Type (Type)
import           WeatherBonus (WeatherBonus)

import qualified Data.List as List

getBreakpoints ::
  GameMaster -> WeatherBonus -> Pokemon -> Pokemon -> [(Float, Int)]
getBreakpoints gameMaster weatherBonus attacker defender =
  let levels = GameMaster.allLevels gameMaster
      levelAndDamageList = map (\ level ->
        let attacker' = PokeUtil.setLevel gameMaster level attacker
            damage = Battle.getDamage weatherBonus
              attacker' (Pokemon.quick attacker') defender
        in (level, damage)) levels
      filtered = filter (\ (level, _) -> level >= Pokemon.level attacker)
        levelAndDamageList
  in List.nubBy (\ (_, d1) (_, d2) -> d1 == d2) filtered

