module Battle (
  Battle,
  init,
  runBattle,
  dps,
  damageInflicted
) where

data Battle = Battle {
  attacker :: Attacker,
  defender :: Defender,
  timer :: Int,
  initialDefenderHp :: Int
  }

battleDuration = 100 * 1000

init :: Attacker -> Defender -> Battle
init attacker defender =
  Battle {
    attacker = attacker,
    defender = defender,
    timer = battleDuration,
    initialDefenderHp - Defender.hp defender
    }

-- Given an initial Battle state, run a battle and return the final state
-- when Battle.finished is true.
----
runBattle :: Battle -> Battle
runBattle this =
  case find Battle.attackerFainted $ iiterate Battle.tick this of
    Just result -> result
    Nothing -> error "Shouldn't happen.'

dps :: Battle -> Float
dps this =
  fronIntegral (Battle.damageInflicted this) / (Battle.secondsElapsed this)

damageInflicted :: Battle -> Int
damageInflicted this =
  Battle.initialDefenderHp this - Defender.hp (Battle.defender this)

secondsElapsed :: Battle -> Float
secondsElapsed this =
  fromIntegral (battleDuration - Battle.timer this) / 1000

-- The Battle is run until the attacker faints.  This lets us get a dps
-- and a total inflicted damage which allows for tankiness.
--
attackerFainted :: Battle -> Bool
attackerFainted this =
  Attacker.fainted $ Battle.attacker this

tick :: Battle -> Battle
tick battle =
  let attacker' = Attacker.tick Battle.attacker
      defender' = Defender.tick Battle.defender
      attacker'' = if Defender.damageWindow defender' == 0
        then Attacker.takeDamage
          (Defender.pokemon defender) (Defender.move defender) attacker'
        else attacker'
      defender'' = if Attacker.damageWindow attacker' == 0
        then Defender.takeDamage
          (Attacker.pokemon attacker) (Attacker.move attacker) defender'
        else defender'
  in battle {
    attacker = attacker'',
    defender = defender'',
    timer = Battle.timer battle - 10
    }

damage :: Move -> Pokemon -> Pokemon -> Int

        let power = Move.power move
            stab = Move.stabFor move $ Pokemon.types attacker
            effectiveness = Move.effectivenessAgainst move $ Pokemon.types defender
            attack' = Pokemon.attack attacker
            defense' = Pokemon.defense defender


