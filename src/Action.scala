// TODO: Finish this!
trait Action[T] {
  def describe(a: Creature, d: Creature): String
  def apply(user: Creature,
            initialTarget: T,
            targetSelector: (T) => T): T
  def canApply(user: Creature, target: Creature): Boolean
}
