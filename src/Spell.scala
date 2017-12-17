// TODO: Implement spells (there may be a common structure with an attack,
// but they can affect allies and/or multiple creatures).
abstract case class Spell[T](name: String) extends Action[T] with Serializable {
  var formula: Formula = _
}

abstract class MonoTargetSpell(override val name: String) extends Spell[Creature](name) {

}

abstract class MultipleTargetsSpell(override val name: String) extends Spell[List[Creature]](name) {

}

object DummyFireSpell extends MultipleTargetsSpell("Fire spell") {
  override def describe(a: Creature, d: Creature): String = s"${a.name} burns ${d.name}"

  override def canApply(user: Creature, target: Creature) : Boolean = true

  override def apply(user: Creature,
                     target: List[Creature],
                     targetSelector: (List[Creature]) => List[Creature]): List[Creature] = {
    return null
  }
}

abstract class HealingSpell[T](override val name: String) extends Spell[T](name) {
    override def canApply(user: Creature, target: Creature) : Boolean = target.getHealthP() < 1
}

abstract class MonoHealingSpell(override val name: String) extends HealingSpell[Creature](name) {
  override def describe(a: Creature, d: Creature): String =  {
    var dName = d.name
    if (a eq d) {
      dName = "itself"
    }

    return s"${a.name} uses ${name} to heal ${dName}"
  }

  override def apply(user: Creature,
                     target: Creature,
                     targetSelector: (Creature) => Creature): Creature = {
    assert(formula != null)

    // TODO?: only display actual heal (if (life + heal > maxLife))
    val fullHeal = formula.compute()

    target.heal(fullHeal)

    val description = describe(user, target)
    println(s"\t${Console.GREEN}${description} for ${fullHeal} hp!${Console.RESET}")

    return target
  }
}

abstract class MultipleHealingSpell(override val name: String) extends HealingSpell[List[Creature]](name) {
  override def describe(a: Creature, d: Creature): String =  {
    var dName = d.name
    if (a eq d) {
      dName = "itself"
    }

    return s"${a.name} heals ${dName}"
  }

  override def apply(user: Creature,
                     target: List[Creature],
                     targetSelector: (List[Creature]) => List[Creature]): List[Creature] = {
    assert(formula != null)

    println(s"\t${Console.GREEN}${user.name} uses ${name}...${Console.RESET}")

    target.foreach(t => {
      // TODO?: only display actual heal (if (life + heal > maxLife))
      val fullHeal = formula.compute()

      t.heal(fullHeal)

      val description = describe(user, t)
      println(s"\t${Console.GREEN}${description} for ${fullHeal} hp!${Console.RESET}")
    })

    return target
  }
}

// NOTE: This should take into account the caster level (CL),
// but I do not want to implement that, so I'll just set the base heal to be
// min(20, maxSpellBaseHeal)
// Same goes for Heal and MassHeall (10 * CL => min(10 * 20, maxBaseSpellHeal))

// Healing spells
object CureLightWounds extends MonoHealingSpell("Cure Light Wounds") {
  formula = new Formula(1, Dice.d8, 5)
}

object CureModerateWounds extends MonoHealingSpell("Cure Moderate Wounds") {
  formula = new Formula(2, Dice.d8, 10)
}

object CureSeriousWounds extends MonoHealingSpell("Cure Serious Wounds") {
  formula = new Formula(3, Dice.d8, 15)
}

object CureCriticalWounds extends MonoHealingSpell("Cure Critical Wounds") {
  formula = new Formula(4, Dice.d8, 20)
}

// NOTE: This should also remove negative effects. But we do not have any!
object Heal extends MonoHealingSpell("Heal") {
  formula = new Formula(0, Dice.d20, 150)
}

object MassCureModerateWounds extends MultipleHealingSpell("Mass Cure Moderate Wounds") {
  formula = new Formula(2, Dice.d8, 20)
}

object MassCureCriticalWounds extends MultipleHealingSpell("Mass Cure Critical Wounds") {
  formula = new Formula(4, Dice.d8, 20)
}

object MassHeal extends MultipleHealingSpell("Mass Heal") {
  formula = new Formula(0, Dice.d20, 200)
}

// Other spells
object AlterSelf extends MonoTargetSpell("Alter-Self") {
  override def describe(a: Creature, d: Creature): String = s"${a.name} uses ${name} to change into a human!"
  override def canApply(user: Creature, target: Creature) : Boolean = target.creatureType != CreatureType.Human
  override def apply(user: Creature,
                     target: Creature,
                     targetSelector: (Creature) => Creature): Creature = {
    assert(user == target)
    user.creatureType = CreatureType.Human

    val description = describe(user, user)
    println(s"\t${description}")

    return user
  }
}