// TODO: Implement spells (there may be a common structure with an attack,
// but they can affect allies and/or multiple creatures).
abstract case class Spell[T](name: String) extends Action[T] {
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

object DummyHealSpell extends MonoTargetSpell("Heal spell") {
  override def describe(a: Creature, d: Creature): String = s"${a.name} heals ${d.name}"

  override def canApply(user: Creature, target: Creature) : Boolean = target.getHealthP() < 1

  override def apply(user: Creature,
                     target: Creature,
                     targetSelector: (Creature) => Creature): Int = {
    return 0
  }
}