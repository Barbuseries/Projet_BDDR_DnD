class Formula(diceCount: Int, dice: Dice, baseDamage: Int) extends Serializable {
  def compute(isCritical: Boolean = false): Int = {
    var result = baseDamage

    var totalDiceCount = diceCount
    if (isCritical) totalDiceCount *= 2

    for (i <- 0 until totalDiceCount) {
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