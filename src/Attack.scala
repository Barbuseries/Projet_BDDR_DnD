import scala.collection.mutable.ArrayBuffer

// TODO: Take into account the creature type when computing the damages.
abstract case class Attack(name: String) extends Serializable {
  protected class DamageFormula(diceCount: Int, dice: Dice, baseDamage: Int) extends Serializable {
    def compute(): Int = {
      var result = baseDamage

      for (i <- 0 until diceCount) {
        result += dice.roll()
      }

      return result
    }

    def computeMin(): Int = {
      var result = diceCount * dice.min() + baseDamage
      return result
    }

    def computeMax(): Int = {
      var result = diceCount * dice.max() + baseDamage
      return result
    }
  }

  var allStrikes: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  var damageFormula: DamageFormula = _

  def describe(a: Creature, d: Creature): String

  def hit(attacker: Creature, defender: Creature): Int = {
    assert(damageFormula != null)

    var total = 0

    println(s"\t${attacker.name} targets ${defender.name}...")

    allStrikes.map(s => {
      if (defender.isAlive()) {
        val roll = Dice.d20.roll()

        if (roll != 1) {
          var pierceDefence = false

          if (roll == 20) {
            pierceDefence = true

            if ((Dice.d20.roll() + s) > defender.armor) {
              println(Console.GREEN + "\tCritical hit!")
              // TODO: ... implement?
              println(s"\t\t${Console.RED_B}but not implemented yet...${Console.RESET}")
            }
            else {
              println(s"\t${attacker.name} pierced ${defender.name}'s defenses!")
            }
          }
          else {
            val totalArmorBreak = roll + s;
            pierceDefence = (totalArmorBreak > defender.armor)
          }

          if (pierceDefence) {
            var damages = damageFormula.compute()
            val description = describe(attacker, defender) + s" for ${damages} hp!"

            defender.takeDamages(damages)
            println(s"\t$description")

            total += damages

            if (!defender.isAlive()) {
              println(s"${Console.RED}\t${defender.name} was slained by ${attacker.name}!${Console.RESET}")
              return total
            }
          }
          else {
            println(s"\t${defender.name} blocked the attack!")
          }
        }
        else {
          println("\tAnd misses miserably...")
        }
      }
    })

    return total
  }

  // TODO: Add a min/max reach attribute (to test if this attack can be used)
  def canHit(creature: Creature): Boolean = {
    return true
  }
}

// Solar
object DancingGreatSword extends Attack("+5 dancing greatsword") {
  allStrikes = ArrayBuffer[Int](35, 30, 25, 20)
  damageFormula = new DamageFormula(3, Dice.d6, 18)

  override def describe(a: Creature, d: Creature): String = {
    val allBodyParts = Array[String]("arm", "leg", "torso", "back", "head")
    val randomPart = scala.util.Random.nextInt(allBodyParts.length)

    return s"${a.name} slashes ${d.name} right in the ${allBodyParts(randomPart)}"
  }
}

object Slam extends Attack("Slam") {
  allStrikes = ArrayBuffer[Int](30)
  damageFormula = new DamageFormula(2, Dice.d8, 13)

  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} slamed into ${d.name}"
  }
}

// Worg Rider
object MWKBattleAxe extends Attack("mwk battleaxe") {
  allStrikes = ArrayBuffer[Int](6)
  damageFormula = new DamageFormula(1, Dice.d8, 2)

  override def describe(a: Creature, d: Creature): String = {
    return s"A swift swing from ${a.name}'s ${name} into ${d.name}"
  }
}

// Warlord
object ViciousFlail extends Attack("+1 vicious flail") {
  allStrikes = ArrayBuffer[Int](20, 15, 10)
  damageFormula = new DamageFormula(1, Dice.d8, 10)

  override def describe(a: Creature, d: Creature): String = {
    return s"A powerful swing from ${a.name}'s ${name} into ${d.name}"
  }
}

object LionShield extends Attack("lion's shield") {
  allStrikes = ArrayBuffer[Int](23)
  damageFormula = new DamageFormula(1, Dice.d4, 6)

  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} bashes ${d.name}"
  }
}

// Barbares Orc
object OrcDoubleAxe extends Attack("+1 orc double axe") {
  allStrikes = ArrayBuffer[Int](19, 14, 9)
  damageFormula = new DamageFormula(1, Dice.d8, 10)

  override def describe(a: Creature, d: Creature): String = {
    return s"A swift swing from ${a.name}'s ${name} into ${d.name}"
  }
}

object OrcDoubleAxe2 extends Attack("+1 orc double axe") {
  allStrikes = ArrayBuffer[Int](17, 12)
  damageFormula = new DamageFormula(1, Dice.d8, 7)

  override def describe(a: Creature, d: Creature): String = {
    return s"A swift swing from ${a.name}'s ${name} into ${d.name}"
  }
}

object Bite extends Attack("bite") {
  allStrikes = ArrayBuffer[Int](12)
  damageFormula = new DamageFormula(1, Dice.d4, 3)

  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name}'bites ${d.name}"
  }
}