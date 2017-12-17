class Formula(diceCount: Int, dice: Dice, base: Int) extends Serializable {
  def compute(isCritical: Boolean = false): Int = {
    var result = base

    var totalDiceCount = diceCount
    if (isCritical) totalDiceCount *= 2

    for (i <- 0 until totalDiceCount) {
      result += dice.roll()
    }

    return result
  }

  def computeMin(): Int = {
    var result = diceCount * dice.min() + base
    return result
  }

  def computeMax(): Int = {
    var result = diceCount * dice.max() + base
    return result
  }

  def computeAverage(): Int = {
    var result = Math.round(diceCount * dice.average() + base)
    return result
  }
}