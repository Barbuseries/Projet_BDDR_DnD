class BreathWeapon(val name: String) extends Action[List[Creature]] with Serializable {
  var damageFormula: Formula = _
  var savingThrowDice: Dice = _

  // My first PS2 game!
  private def buryDead(deadOrAlive2: List[Creature]): List[Creature] = {
    val result = deadOrAlive2.filter(_.isAlive())

    if (result.length == 0) return null
    return result
  }

  override def apply(attacker: Creature,
                     initialCreatures: List[Creature] = null,
                     targetSelector: (List[Creature]) => List[Creature]= null): List[Creature] = {
    assert(damageFormula != null)
    assert(savingThrowDice != null)

    var defenders = initialCreatures

    assert((initialCreatures != null) || (targetSelector != null))

    var oldDefenders = defenders

    println(s"${attacker.name} uses ${name}...")

    defenders.foreach(d => {
      if (defenders == null)
        defenders = targetSelector(oldDefenders)

      if (defenders == null)
        return defenders

      defenders.foreach((d) => assert(d.isAlive()))

      // NOTE/FIXME: I can not wrap my mind around a Reflex save's throw calculation.
      // So I'm just going for this.... Sorry.
      val saved = (savingThrowDice.roll() < 10)
      var fullDamages = damageFormula.compute()

      var description = describe(attacker, d)

      if (saved) {
        fullDamages /= 2
        description += s" (saved)"
      }

      val damages = fullDamages

      if (damages > 0) description += s" for ${damages} hp!"
      else description += s"... but it did not do anything!"

      println(s"\t$description")

      if (damages > 0) d.takeDamages(damages, attacker)

      if (targetSelector != null) {
        oldDefenders = buryDead(defenders)
        defenders = null
      }
      else {
        defenders = buryDead(defenders)
        oldDefenders = defenders
      }
    })

    return oldDefenders
  }

  override def describe(a: Creature, d: Creature): String = s"${a.name} hits ${d.name}"

  override def canApply(user: Creature, target: Creature): Boolean = {
    // Just because the wiki says so *wink*
    return user != target
  }
}