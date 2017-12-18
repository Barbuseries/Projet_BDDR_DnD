import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import Context.{Mapper, Reducer}
import Main.World
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{VertexId}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Relationship extends Enumeration {
  val Ally, Enemy, All = Value
}

object CreatureType extends Enumeration {
  val Angel, Orc, Dragon, Human = Value
}

abstract class Creature(val name : String) extends Serializable {
  var creatureType: CreatureType.Value = _

  var initiative: Int = 0

  var healthFormula: Formula = _
  var maxHealth: Int = 0
  var health: Int = 0

  var armor: Int = 0

  // TODO: Look at what breaks damageReduction
  var damageReduction: Int = 0
  var spellReduction: Int = 0

  // TODO: This is only set for the solar (and maybe other angels),
  // does it really belong here?
  var regeneration: Int = 0

  // NOTE: This is actually only useful when doing Angel Slayer -> Angel attack.
  var typeBonusOnAttack: List[(CreatureType.Value, Int)] = List()

  var allAttacks: List[List[Action[_]]] = List()
  // NOTE: spells are removed after they have been use
  var allSpells: ArrayBuffer[Spell[_]] = ArrayBuffer.empty[Spell[_]]

  def init(): Unit = {
    assert(healthFormula != null)

    maxHealth = healthFormula.compute()
    health = maxHealth
  }

  // Because copy and clone do not work as I would like...
  def bulldozerCopy(): Creature = {
    val filename = s"/tmp/$name.dat"
    val os = new ObjectOutputStream(new FileOutputStream(filename))
    os.writeObject(this)
    os.close()

    val is = new ObjectInputStream(new FileInputStream(filename))
    val obj = is.readObject().asInstanceOf[Creature]
    is.close()

    return obj
  }

  protected def think(context: Context): Boolean

  def play(id: VertexId, graph: World, store: Broadcast[CreatureStore.type]) : Unit = {
    println(s"$name ($health / $maxHealth) is playing...")
    var played = false

    regenerate()

    val context = new Context(id, graph, store)

    played = think(context)

    if (!played) {
      println("\tBut can not do anything...")
    }
  }

  def isAlive(): Boolean = {
    return health > 0
  }

  def attackBonusAgainst(creatureType: CreatureType.Value): Int = {
    val result = typeBonusOnAttack.find(_._1 == creatureType)

    if (!result.isEmpty) return result.head._2
    else return 0
  }

  def takeDamages(amount: Int, attacker: Creature): Unit = {
    // TODO: Special message if we happen to have amount == 0?
    // (This is why there is this assert, in case it _does_
    // happen and I forgot to do it.
    assert(amount > 0)
    health -= amount

    if (health <= 0) {
      health = 0

      println(s"\t${Console.RED}${name} was slain by ${attacker.name}!${Console.RESET}")
    }
  }

  def heal(amount: Int): Unit = {
    // TODO: Special message if we happen to have amount == 0?
    // (This is why there is this assert, in case it _does_
    // happen and I forgot to do it.
    assert(amount > 0)

    health += amount

    if (health > maxHealth) health = maxHealth
  }

  def attack[T](attacks: List[Action[_]], target: T, targetSelector: (T) => T): Unit = {
    var currentTarget = target

    if (targetSelector != null) {
      attacks.foreach((a) => {
        currentTarget = targetSelector(a.asInstanceOf[Action[T]].apply(this, currentTarget, targetSelector))
      })
    }
    else {
      attacks.foreach((a) => {
        currentTarget = a.asInstanceOf[Action[T]].apply(this, currentTarget, null)
      })
    }
  }

  // FIXME: Oh boy...
  // It's late. I should not be doing this.
  def attackAccordingTo[T](targetSelector: (T) => T): Boolean = {
    var firstCreature = targetSelector(null.asInstanceOf[T])
    if (firstCreature == null) return false

    var checkAllValid: (Action[T], T) => Boolean = null

    if (firstCreature.isInstanceOf[List[_]]) {
      val checkAllValid = (a: Action[List[_]], l: List[Creature]) => l.map(a.canApply(this, _)).reduce((a, b) => a && b)
    }
    else {
      checkAllValid = (a: Action[T], c: T) => a.canApply(this, c.asInstanceOf[Creature])
    }

    // Can apply all attacks in the same list
    var validAttacks = allAttacks.filter(_.isInstanceOf[List[Action[T]]]).map(_.asInstanceOf[List[Action[T]]])
    validAttacks = validAttacks.filter(_.map(checkAllValid(_, firstCreature)).reduce((a, b) => a && b))

    if (validAttacks.length == 0) return false

    // TODO: Can be changed to rank based on min/max/average damages
    val choosenAttack = validAttacks(scala.util.Random.nextInt(validAttacks.length))
    attack(choosenAttack, firstCreature, targetSelector)

    return true
  }

  protected def useSpell[T](spell: Spell[T], target: T, targetSelector: (T) => T): Unit = {
    val index = allSpells.indexOf(spell)
    assert(index != -1)
    spell.apply(this, target, targetSelector)

    allSpells.remove(index)
  }

  def regenerate(): Unit = {
    if ((health != maxHealth) && (regeneration != 0)) {
      println(s"\t${Console.GREEN}${name} regenerates ${regeneration} hp.${Console.RESET}")
      heal(regeneration)
    }
  }

  def getHealthP(): Float = {
    val result = (1.0f * health) / maxHealth
    return result
  }

  // Ask entities with the given relationship what their life is. Return the one with the lowest health.
  private def findWeakest(context: Context, relationship: Relationship.Value, validator: (Creature) => Boolean)(): Creature = {
    var tempResult: Result[(Int, Int)] = null

    val mapper: Mapper[(Int, Int)] = (e, creature, key) => {
      if (validator(creature)) e.sendToSrc((key, creature.health))
    }

    val reducer: Reducer[(Int, Int)] = (a, b) => if (a._2 > b._2) b else a

    if (relationship == Relationship.All)
      tempResult = context.onAll[(Int, Int)](mapper, reducer)
    else
      tempResult = context.onLinked[(Int, Int)]((e) => e.attr == relationship, mapper, reducer)

    if (tempResult == null) return null

    val result = context.store.value.get(tempResult.value._1)

    return result
  }

  protected def findWeakestEnemy(context: Context, validator: (Creature) => Boolean = (c) => true)(): Creature =
    findWeakest(context, Relationship.Enemy, validator)

  protected def findRandomEnemies(context: Context, count: Int) : List[Creature] = {
    val tempResult = context.onEnemies[List[Int]](
      (e, creature, key) => {
        e.sendToSrc(List(key))
      },
      (a, b)  => a ++ b)

    if (tempResult == null) return null

    val enemies = Random.shuffle(tempResult.value).take(count)

    val result = enemies.map(context.store.value.get(_))
    return result
  }

  protected def findRandomEnemy(context: Context)() : Creature = {
    val tempResult = context.onEnemies[List[Int]](
      (e, creature, key) => {
          e.sendToSrc(List(key))
      },
      (a, b)  => a ++ b)

    if (tempResult == null) return null

    val enemies = tempResult.value
    val randomIndex = new Dice(enemies.length).roll() - 1

    val result = enemies(randomIndex)
    return context.store.value.get(result)
  }

  protected def findByType(context: Context, creatureType: CreatureType.Value): Creature = {
    val result = context.onEnemies[Int]((e, creature, key) => {
      creature.creatureType match {
        case `creatureType` => e.sendToSrc(key)
        case _ =>
      }
    },
      (a, b) => a)

    if (result == null) return null
    return context.store.value.get(result.value)
  }

  protected def findDragon(context: Context): Creature = findByType(context, CreatureType.Dragon)
  protected def findHuman(context: Context): Creature = findByType(context, CreatureType.Human)

  protected def addSpell(s: Spell[_], count: Int): Unit = {
    allSpells ++= (1 to count).map(_ => s)
  }

  protected def addSpell(s: Spell[_]): Unit = {
    allSpells += s
  }
}

object Bestiary {
  // Make own think method
  abstract class Angel(override val name: String) extends Creature(name) {
    creatureType = CreatureType.Angel

    val minRatioToUseMassHeal = 0.3f

    private def findWeakestAngelSlayer(context: Context): Creature = {
        val result = context.onEnemies[(Int, Int)]((e, creature, key) => {
          creature match {
            case slayer: AngelSlayer => e.sendToSrc(key, slayer.health)
            case _ =>
          }
        },
          (a, b) => if (a._2 > b._2) b else a)

      if (result == null) return null
      return context.store.value.get(result.value._1)
    }

    protected def healAllies(context: Context): Boolean = {
      // TODO: Implement something based on either a spell type or a spell target (allies and or enemies)
      val healingSpells = allSpells.filter(_.isInstanceOf[HealingSpell[_]])
      if (healingSpells.length == 0) return false

      var dragonThreat = 0

      if(findDragon(context) != null) {
        dragonThreat = Math.round(AcidBreath.damageFormula.computeAverage() * 1.25f)
      }

      val includeSelf = true
      val tempResult = context.onAllies[List[Int]](
        (e, creature, key) => {
          e.sendToSrc(List(key))
        },
        (a, b)  => a ++ b,
        includeSelf)

      // Should never happen as _we_ are included
      assert(tempResult != null)

      val allies = tempResult.value.map((k) => context.store.value.get(k))
      val alliesToHeal = allies.filter((a) => {
        val healRatio = a.getHealthP()
        val needHeal = (a.health <= dragonThreat) || (healRatio < 0.33f)

        needHeal
      })

      if (alliesToHeal.length == 0) return false


      val monoHeals = healingSpells.filter(_.isInstanceOf[MonoHealingSpell])
      val massHeals = healingSpells.filter(_.isInstanceOf[MultipleHealingSpell])

      val ratio = (1.0f * alliesToHeal.length / allies.length)

      // Youhou, heuristics!
      val useMassHeal = (((massHeals.length != 0) && (ratio >= minRatioToUseMassHeal)) ||
                         (monoHeals.length == 0))

      // TODO: Get the most useful
      // TODO: Maybe limit the number of allies you can heal at once
      if (useMassHeal) {
        val spell = massHeals(0).asInstanceOf[MultipleHealingSpell]
        val target = allies

        useSpell(spell, target, null)
      }
      else {
        val spell = monoHeals(0).asInstanceOf[MonoHealingSpell]
        val target = alliesToHeal.minBy(_.health)

        useSpell(spell, target, null)
      }

      return true
    }

    override protected def think(context: Context): Boolean = {
      if ((Main.fight == 1) && (Main.round < Main.roundFightStarts)) {
        if (findHuman(context) == null) {
          println(s"\t${name} is wary...")
        }
        else {
          println(s"\t${name} sees a frightened villager!")
        }

        return true
      }

      if(healAllies(context)) return true

      // TODO: Change to use a custom strength evaluation function
      val strategy = (previousTarget : Creature) => {
        if (previousTarget != null) {
          previousTarget
        }
        else {
          var target = findWeakestAngelSlayer(context)

          if (target == null) {
            /*target = this match {
              case solar: Solar => findDragon(context)
              case _ => null
            }*/

            if (target == null) {
              // Can not attack humans! (altered dragon)
              // It would not get attacked anyway,
              // because the fight does not start before the dragon is a dragon again.
              target = findWeakestEnemy(context, _.creatureType != CreatureType.Human)
            }
          }

          target
        }
      }

      if (attackAccordingTo(strategy)) return true

      return false
    }
  }

  case class Solar() extends Angel("Solar") {
    healthFormula = new Formula(22, Dice.d10, 242)

    initiative = 9
    armor = 44
    regeneration = 15

    damageReduction = 15
    spellReduction = 34

    allAttacks = List(List(DancingGreatSword)/*, SolarSlam*/, List(CompositeLongbow))

    addSpell(CureLightWounds, 3)
    addSpell(CureModerateWounds, 2)
    addSpell(CureSeriousWounds)
    addSpell(CureCriticalWounds, 3)
    addSpell(Heal, 3)

    addSpell(MassCureModerateWounds)
    addSpell(MassCureCriticalWounds, 2)
    addSpell(MassHeal)
  }

  case class Planetar() extends Angel("Planetar") {
    healthFormula = new Formula(17, Dice.d10, 136)

    initiative = 8
    armor = 32
    regeneration = 10

    damageReduction = 10
    spellReduction = 27

    allAttacks = List(List(HolyGreatSword)/*, PlanetarSlam*/, List(CompositeLongbow))

    addSpell(CureLightWounds, 4)
    addSpell(CureModerateWounds, 2)
    addSpell(CureSeriousWounds, 2)
    addSpell(Heal)

    addSpell(MassCureModerateWounds)
  }

  case class MovanicDeva() extends Angel("Movanic Deva") {
    healthFormula = new Formula(12, Dice.d10, 60)

    initiative = 7
    armor = 24

    damageReduction = 10
    spellReduction = 21

    allAttacks = List(List(FlamingGreatSword), List(CompositeLongbow))

    // NOTE: It says "7/day"
    addSpell(CureSeriousWounds, 7)
  }

  case class AstralDeva() extends Angel("Astral Deva") {
    healthFormula = new Formula(15, Dice.d10, 90)

    initiative = 8
    armor = 29

    damageReduction = 10
    spellReduction = 25

    allAttacks = List(List(DisruptingWarhammer)/*, AstralSlam*/, List(CompositeLongbow))

    addSpell(CureSeriousWounds, 7)
    addSpell(Heal)
  }

  case class GreenGreatWyrmDragon() extends Creature("Green Great Wyrm Dragon") {
    healthFormula = new Formula(27, Dice.d12, 216)

    creatureType = CreatureType.Dragon

    initiative = 2
    armor = 37

    damageReduction = 20
    spellReduction = 31

    allAttacks = List(List(DragonBite, Claw, Wings, TailSlap), List(AcidBreath))

    addSpell(AlterSelf)

    val roundsBetweenAttacks = 1
    var remainingRoundsBeforeAttack = 0

    private def roundWhenCloseEnough(): Int = {
      return Main.roundFightStarts - 1
    }

    private def getSolar(context: Context): Creature = {
      var result = context.onEnemies[Int](
        (e, creature, key) => {
          creature match {
            case solar: Solar => e.sendToSrc(key)
            case _ =>
          }
        },
          (a, b) => a)

      assert(result != null)

      if (result == null) return null
      return context.store.value.get(result.value)
    }

    private def thinkFirstRound(context: Context): Boolean = {
      useSpell(AlterSelf, this, null)

      return true
    }

    private def resetAttackTimer(): Unit = {
      remainingRoundsBeforeAttack = roundsBetweenAttacks
    }

    private def thinkWhileHuman(context: Context) : Boolean = {
      val solar: Creature = getSolar(context)

      if (Main.round != roundWhenCloseEnough()) {
        println(s"\t${name} gets closer to ${solar.name}...")
      }
      else {
        println(s"\t${name} changes back to a dragon!")
        creatureType = CreatureType.Dragon

        attack(allAttacks.head, solar, (creature: Creature) => creature)

        println(s"\t${name} takes off!")
      }

      resetAttackTimer()

      return true
    }

    private def thinkRemainingFight(context: Context): Boolean = {
      if (remainingRoundsBeforeAttack == 0) {
        val acidBreath = allAttacks(1)
        val enemies = findRandomEnemies(context, 3)

        attack(acidBreath, enemies, null)

        resetAttackTimer()

        return enemies.length != 0
      }
      else {
        println(s"\t${name} flies around the battlefield.")
        remainingRoundsBeforeAttack -= 1

        return true
      }
    }

    override protected def think(context: Context): Boolean = {
      val played = Main.round match {
        case 0 => thinkFirstRound(context)
        case x if (x <= roundWhenCloseEnough()) => thinkWhileHuman(context)
        case _ => thinkRemainingFight(context)
      }

      return played
    }

    def healMe(): Boolean = {
      if (getHealthP() < 0.5f) {
        if (new Dice(42).roll() == 42) return true
      }

      return false
    }
  }

  abstract class Orc(override val name: String) extends Creature(name) {
    creatureType = CreatureType.Orc

    override protected def think(context: Context): Boolean = {
      val strategy = (c: Creature) => findRandomEnemy(context)

      if (Main.fight == 1) {
        val stopHidingRound = Main.roundFightStarts - 1
        if (Main.round < stopHidingRound) {
          println(s"\t${Console.CYAN}${name} hides in a bush...${Console.RESET}")
          return true
        }
        else if (Main.round == stopHidingRound) {
          println(s"\t${Console.CYAN}${name} emerges from its bush!${Console.RESET}")
          return true
        }
        else if (Main.round < Main.roundFightIsMelee) {
          println(s"\t${Console.CYAN}${name} rushes its prey!${Console.RESET}")
        }
      }
      else if (Main.round < Main.roundFightIsMelee) {
        println(s"\t${Console.CYAN}${name} rushes its prey!${Console.RESET}")
      }

      return attackAccordingTo(strategy)
    }
  }

  case class OrcBarbarian() extends Orc("Orc Barbarian") {
    healthFormula = new Formula(4, Dice.d12, 16)

    initiative = 1
    armor = 15

    allAttacks = List(List(GreatAxe), List(OrcThrowingAxe))
  }

  case class AngelSlayer() extends Orc("Angel Slayer") {
    healthFormula = new Formula(15, Dice.d10, 25)

    initiative = 7
    armor = 26

    allAttacks = List(List(DoubleAxe, DoubleAxe2), List(MWKCompositeLongbow2))
    addSpell(CureModerateWounds)

    typeBonusOnAttack = List((CreatureType.Angel, 8))

    override protected def think(context: Context): Boolean = {
      if (allSpells.length != 0) {
        val dragon = findDragon(context)

        if ((dragon != null) && (dragon.asInstanceOf[GreenGreatWyrmDragon].healMe())) {
            // NOTE: This is actually never reached, because angels only start attacking the dragon
            // when all angel slayers have been defeated.
            val healSpell = allSpells(0).asInstanceOf[MonoHealingSpell]
            useSpell(healSpell, dragon, null)

            return true
        }
      }

      return super.think(context)
    }
  }

  case class WorgRider() extends Orc("Worg Rider") {
    healthFormula = new Formula(2, Dice.d10, 2)

    initiative = 2
    armor = 18

    allAttacks = List(List(MWKBattleAxe), List(ShortBow))
  }

  case class Warlord() extends Orc("Warlord") {
    healthFormula = new Formula(13, Dice.d10, 65)

    initiative = 2
    armor = 27

    allAttacks = List(List(ViciousFlail, LionShield), List(ViciousFlail2), List(MWKThrowingAxe))
  }

  case class BarbaresOrc() extends Orc("Barbares Orc") {
    healthFormula = new Formula(11, Dice.d12, 65)

    initiative = 4
    armor = 17

    damageReduction = 3

    allAttacks = List(List(OrcDoubleAxe), List(OrcDoubleAxe2, OrcDoubleAxe3, OrcBite), List(MWKCompositeLongbow))
  }
}