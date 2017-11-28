import scala.collection.mutable.ArrayBuffer

// TODO: Take into account the creature type when computing the damages.
abstract case class Attack(name: String) extends Serializable {
  // TODO: Handle dices
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

    // NOTE: We could just compute the total damages and _then_ hit the creature,
    // instead of hitting it every time.
    println(s"\t${attacker.name} targets ${defender.name}...")

    allStrikes.map(s => {
      if (defender.isAlive()) {
        // TODO: If Dice.d20.roll() == 1, it is always a miss
        //                          == 20, it may be a critical if a second computation of totalArmorBreak (dice + ) > defender.armor
        //                          otherwhise, it is a regular hit
        val totalArmorBreak = Dice.d20.roll() + s;

        if (totalArmorBreak > defender.armor) {
          var damages = damageFormula.compute()
          val description = describe(attacker, defender) + s" for ${damages} hp!"

          defender.takeDamages(damages)
          println(s"\t$description")

          total += damages

          if (!defender.isAlive()) {
            println(s"${Console.RED}\t${defender.name} was slained by ${attacker.name}!${Console.BLACK}")
            return total
          }
        }
        else {
          println(s"\t${defender.name} blocked the attack!")
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

object MWKBattleAxe extends Attack("mwk battleaxe +6") {
  allStrikes = ArrayBuffer[Int](0)
  damageFormula = new DamageFormula(1, Dice.d8, 2)

  override def describe(a: Creature, d: Creature): String = {
    return s"A swift swing from ${a.name}'s ${name} into ${d.name}"
  }
}
