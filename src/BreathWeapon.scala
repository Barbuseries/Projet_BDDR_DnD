class BreathWeapon(val name: String) extends Action[List[Creature]] {
  var damageFormula: Formula = _
  var savingThrowDice: Dice = _

  // Yes. Yes it is.
  private def buryDead(deadOrAlive: List[Creature]): List[Creature] = {
    val result = deadOrAlive.filter(_.isAlive())

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

      val damages = fullDamages - d.spellReduction

      description += s" for ${damages} hp! (${Console.RED}$fullDamages${Console.RESET} - ${Console.BLUE}${d.spellReduction}${Console.RESET})"

      println(s"\t$description")

      d.takeDamages(damages, attacker)

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