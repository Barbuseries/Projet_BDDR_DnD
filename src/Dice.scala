object Dice {
  val dice = scala.util.Random

  def dn(n: Int): Int = {
    return dice.nextInt(n) + 1
  }

  def d20(): Int = {
    return dn(20)
  }
}
