// TODO: Implement spells (there may be a common structure with an attack,
// but they can affect allies and/or multiple creatures).
abstract case class Spell[T](name: String) extends Action[T] {
  var damageFormula: Formula = _
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
                     targetSelector: (List[Creature]) => List[Creature]): Int = {
    return 0
  }
}

abstract class HealingSpell[T](override val name: String) extends Spell[T](name) {
  override def describe(a: Creature, d: Creature): String = s"${a.name} heals ${d.name}"
  override def canApply(user: Creature, target: Creature) : Boolean = target.getHealthP() < 1
}

abstract class MonoHealingSpell(override val name: String) extends HealingSpell[Creature](name) {
  override def apply(user: Creature,
                     target: Creature,
                     targetSelector: (Creature) => Creature): Int = {
    assert(damageFormula != null)

    // TODO?: only display actual heal (if (life + heal > maxLife))
    val fullHeal = damageFormula.compute()

    target.heal(fullHeal)

    val description = describe(user, target)
    println(s"\t${Console.GREEN}${description} for ${fullHeal} hp!${Console.RESET}")

    return fullHeal
  }
}

object CureLightWounds extends MonoHealingSpell("Cure light wounds") {
  damageFormula = new Formula(1, Dice.d8, 5)
}

object CureModerateWounds extends MonoHealingSpell("Cure moderate wounds") {
  damageFormula = new Formula(2, Dice.d8, 10)
}

object CureSeriousWounds extends MonoHealingSpell("Cure serious wounds") {
  damageFormula = new Formula(3, Dice.d8, 15)
}

object CureCriticalWounds extends MonoHealingSpell("Cure critical wounds") {
  damageFormula = new Formula(4, Dice.d8, 20)
}