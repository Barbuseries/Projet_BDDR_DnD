import scala.collection.mutable.ArrayBuffer

// TODO: Take into account the creature type when computing the damages.
abstract case class Attack(name: String) extends Action[Creature] with Serializable {
  var allStrikes: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  var damageFormula: Formula = _

  override def apply(attacker: Creature,
          initialCreature: Creature = null,
          targetSelector: (Creature) => Creature = null): Int = {
    assert(damageFormula != null)

    var total = 0
    var defender = initialCreature

    assert((initialCreature != null) || (targetSelector != null))

    var oldDefender = defender
    allStrikes.foreach(s => {
      if (defender == null)
        defender = targetSelector(oldDefender)

      if (defender == null)
         return total

      assert(defender.isAlive())

      println(s"\t${attacker.name} targets ${defender.name}...")

      val roll = Dice.d20.roll()

      if (roll != 1) {
        var pierceDefence = false
        var isCritical = false

        if (roll == 20) {
          pierceDefence = true

          if ((Dice.d20.roll() + s) > defender.armor) {
            isCritical = true

            println(s"\t${Console.BLUE}Critical hit!${Console.RESET}")
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
          var fullDamages = damageFormula.compute(isCritical)

          var damages = fullDamages - defender.damageReduction

          var description = describe(attacker, defender)

          if (damages > 0) {
            description += s" for ${damages} (${Console.RED}${fullDamages}${Console.RESET} - ${Console.BLUE}${defender.damageReduction}${Console.RESET}) hp!"

            defender.takeDamages(damages)
            total += damages
          }
          else
            description += s"... but it bounced off!"

          println(s"\t$description")

          if (!defender.isAlive()) {
            println(s"${Console.RED}\t${defender.name} was slained by ${attacker.name}!${Console.RESET}")
          }
        }
        else {
          println(s"\t${defender.name} blocked the attack!")
        }
      }
      else {
        println(s"\t${attacker.name} misses miserably...")
      }

      // Reset the defender after each strike (if the creature is non constant)
      if (targetSelector != null) {
        if (defender.isAlive())
          oldDefender = defender
        else
          oldDefender = null

        defender = null
      }
    })

    return total
  }

  // TODO: Add a min/max reach attribute (to test if this attack can be used)
  def canApply(attacker: Creature, defender: Creature): Boolean = {
    return true
  }
}

class Axe(name: String) extends Attack(name) {
  override def describe(a: Creature, d: Creature): String = {
    return s"A swift swing from ${a.name}'s ${name} into ${d.name}"
  }
}

class Bite(name: String = "bite") extends Attack(name) {
  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} bites ${d.name}"
  }
}

class Sword(name: String) extends Attack(name) {
  override def describe(a: Creature, d: Creature): String = {
    val allBodyParts = Array[String]("arm", "leg", "torso", "back",
      "head", "shoulder", "thigh", "knee")
    val randomPart = scala.util.Random.nextInt(allBodyParts.length)

    return s"${a.name} slashes ${d.name} right in the ${allBodyParts(randomPart)}"
  }
}

class Slam(name: String = "slam") extends Attack(name) {
  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} slamed into ${d.name}"
  }
}

// Solar
object DancingGreatSword extends Sword("+5 dancing greatsword") {
  allStrikes = ArrayBuffer[Int](35, 30, 25, 20)
  damageFormula = new Formula(3, Dice.d6, 18)
}

object SolarSlam extends Slam {
  allStrikes = ArrayBuffer[Int](30)
  damageFormula = new Formula(2, Dice.d8, 13)
}

// Worg Rider
object MWKBattleAxe extends Axe("mwk battleaxe") {
  allStrikes = ArrayBuffer[Int](6)
  damageFormula = new Formula(1, Dice.d8, 2)
}

// Warlord
object ViciousFlail extends Attack("+1 vicious flail") {
  allStrikes = ArrayBuffer[Int](20, 15, 10)
  damageFormula = new Formula(1, Dice.d8, 10)

  override def describe(a: Creature, d: Creature): String = {
    return s"A powerful swing from ${a.name}'s ${name} into ${d.name}"
  }
}

object LionShield extends Attack("lion's shield") {
  allStrikes = ArrayBuffer[Int](23)
  damageFormula = new Formula(1, Dice.d4, 6)

  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} bashes ${d.name}"
  }
}

// Barbares Orc
object OrcDoubleAxe extends Axe("+1 orc double axe") {
  allStrikes = ArrayBuffer[Int](19, 14, 9)
  damageFormula = new Formula(1, Dice.d8, 10)
}

object OrcDoubleAxe2 extends Axe("+1 orc double axe") {
  allStrikes = ArrayBuffer[Int](17, 12)
  damageFormula = new Formula(1, Dice.d8, 7)
}

object OrcBite extends Bite {
  allStrikes = ArrayBuffer[Int](12)
  damageFormula = new Formula(1, Dice.d4, 3)
}

// Orc Barbarian
object GreatAxe extends Axe("great axe") {
  allStrikes = ArrayBuffer[Int](11)
  damageFormula = new Formula(1, Dice.d12, 10)
}

// Green Great Wyrm Dragon
object DragonBite extends Bite {
  allStrikes = ArrayBuffer[Int](33)
  damageFormula = new Formula(4, Dice.d8, 21)
}

// TODO/FIXME: The wiki says the dragon can do _two_ claws.
// Currently, the only way to do this is to have two strikes.
// In the end, it is the same. But who knows...
object Claw extends Attack("claw") {
  allStrikes = ArrayBuffer[Int](33, 33)
  damageFormula = new Formula(4, Dice.d6, 14)

  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} rips ${d.name} apart"
  }
}

// TODO/FIXME: See Claw
object Wings extends Attack("wing") {
  allStrikes = ArrayBuffer[Int](31, 31)
  damageFormula = new Formula(2, Dice.d8, 7)

  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} <insert wing joke here> ${d.name}"
  }
}

object TailSlap extends Attack("tail slap") {
  allStrikes = ArrayBuffer[Int](31)
  damageFormula = new Formula(4, Dice.d6, 21)

  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} ${name}s ${d.name}"
  }
}

// Planetar
object HolyGreatSword extends Sword("+3 holy greatsword") {
  allStrikes = ArrayBuffer[Int](27, 22, 17)
  damageFormula = new Formula(3, Dice.d6, 15)
}

object PlanetarSlam extends Slam {
  allStrikes = ArrayBuffer[Int](24)
  damageFormula = new Formula(2, Dice.d8, 12)
}

// Movanic Deva
object FlamingGreatSword extends Sword("+1 flaming greatsword") {
  allStrikes = ArrayBuffer[Int](17, 12, 7)
  damageFormula = new Formula(2, Dice.d6, 7) // TODO: It says: 'plus 1d6 fire'
}

// Astral Deva
object DisruptingWarhammer extends Attack("+2 disrupting warhammer") {
  allStrikes = ArrayBuffer[Int](26, 21, 16)
  damageFormula = new Formula(1, Dice.d8, 14) // TODO: It says: 'plus stun'

  override def describe(a: Creature, d: Creature): String = {
    return s"${a.name} stomps ${d.name}"
  }
}

object AstralSlam extends Slam {
  allStrikes = ArrayBuffer[Int](23)
  damageFormula = new Formula(1, Dice.d8, 12)
}

// Angel Slayer
object DoubleAxe extends Axe("+1 good outsider-bane orc double axe") {
  allStrikes = ArrayBuffer[Int](21, 16, 11)
  damageFormula = new Formula(1, Dice.d8, 7)
}

object DoubleAxe2 extends Axe("+1 orc double axe") {
  allStrikes = ArrayBuffer[Int](21, 16, 11)
  damageFormula = new Formula(1, Dice.d8, 7)
}