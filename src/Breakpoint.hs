module Breakpoint (
   getBreakpoints,
) where

import qualified Battle
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Move
import qualified Pokemon
import           Pokemon (Pokemon)
import qualified PokeUtil
import           Type (Type)
import           WeatherBonus (WeatherBonus)

import qualified Data.List.Extra

getBreakpoints :: GameMaster -> WeatherBonus -> Float ->
  Pokemon -> Pokemon -> [(Float, Int, Float)]
getBreakpoints gameMaster weatherBonus friendBonus attacker defender =
  let levels = GameMaster.allLevels gameMaster
      quick = Pokemon.quick attacker
      levelAndDamageList = map (\ level ->
        let attacker' = PokeUtil.setLevel gameMaster level attacker
            damage = Battle.getDamage weatherBonus friendBonus
              attacker' quick defender
            dps = fromIntegral damage / Move.duration quick
        in (level, damage, dps)) levels
      filtered = filter (\ (level, _, _) -> level >= Pokemon.level attacker)
        levelAndDamageList
  in Data.List.Extra.nubOn (\ (_, damage, _) -> damage) filtered
