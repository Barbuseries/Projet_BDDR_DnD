// TODO: Finish this!
trait Action[T] {
  def describe(a: Creature, d: Creature): String
  def apply(user: Creature,
            initialTarget: T,
            targetSelector: (T) => T): Creature
  def canApply(user: Creature, target: Creature): Boolean
}
