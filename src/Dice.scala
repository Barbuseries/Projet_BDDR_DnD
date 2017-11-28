class Dice(n: Int) extends Serializable {
  val dice = scala.util.Random

  def roll(): Int = {
    return dice.nextInt(n) + 1
  }

  def min(): Int = {
    return 1
  }

  def max(): Int = {
    return n
  }

  def average(): Float = {
    return (n + 1) / 2
  }
}

// Common dices
object Dice {
  case object d3  extends Dice(3)
  case object d6  extends Dice(6)
  case object d8  extends Dice(8)
  case object d10 extends Dice(10)
  case object d20 extends Dice(20)
}