import scala.collection.mutable.ArrayBuffer

case class Attack extends Serializable {
  // TODO: Handle dices
  protected case class Strike (armorBreak: Int, baseDamage: Int) extends Serializable {
    def computeDamages(): Int = {
      return baseDamage
    }
  }

  // TODO: Add a min/max reach attribute (to test if this attack can be used)
  // (See canHit)

  var allStrikes: ArrayBuffer[Strike] = ArrayBuffer.empty[Strike]

  def parse(str: String): Unit = {
    val result = null

    allStrikes += new Strike(20, 40)
  }

  def hit(creature: Creature): Unit = {
    // NOTE: We could just compute the total damages and _then_ hit the creature,
    // instead of hitting it every time.
    allStrikes.map(s => {
      if (creature.isAlive()) {
        val totalArmorBreak = Dice.d20() + s.armorBreak;

        if (totalArmorBreak < creature.armor) return;

        val damages = s.computeDamages()

        creature.takeDamages(damages)
      }
    })
  }

  def canHit(creature: Creature): Boolean = {
    return true
  }
}

