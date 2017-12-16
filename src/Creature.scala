import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{Graph, VertexId}

import scala.collection.mutable.ArrayBuffer

abstract class Creature(val name : String) extends Serializable {
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

  var allAttacks: ArrayBuffer[Attack] = ArrayBuffer.empty[Attack]
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

  protected def think(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]): Boolean = {
    // TODO (way later): Ask allies if they need anything
    if(healAllies(id, graph, store)) return true

    // TODO: Change to use a custom strength evaluation function
    val strategy = (previousTarget : Creature) => {
      if (previousTarget != null) {
        previousTarget
      }
      else {
        findWeakestEnemy(id, graph, store)
      }
    }

    if (attack(strategy)) return true

    return false
  }

  def play(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]) : Unit = {
    println(s"$name ($health) is playing...")
    var played = false

    regenerate()

    played = think(id, graph, store)

    if (!played) {
      println("\tBut can not do anything...")
    }
  }

  def isAlive(): Boolean = {
    return health > 0
  }

  def takeDamages(amount: Int): Unit = {
    // TODO: Special message if we happen to have amount == 0?
    // (This is why there is this assert, in case it _does_
    // happen and I forgot to do it.
    assert(amount > 0)
    health -= amount

    if (health < 0) health = 0
  }

  def heal(amount: Int): Unit = {
    // TODO: Special message if we happen to have amount == 0?
    // (This is why there is this assert, in case it _does_
    // happen and I forgot to do it.
    assert(amount > 0)

    health += amount

    if (health > maxHealth) health = maxHealth
  }

  def attack(targetSelector: (Creature) => Creature): Boolean = {
    val firstCreature = targetSelector(null)
    if (firstCreature == null) return false

    val validAttacks = allAttacks.filter(_.canApply(this, firstCreature))
    if (validAttacks.length == 0) return false

    // TODO: Can be changed to rank based on min/max/average damages
    val choosenAttack = validAttacks(scala.util.Random.nextInt(validAttacks.length))

    var damages = choosenAttack.apply(this, firstCreature, targetSelector)

    return true
  }

  protected def healAllies(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]): Boolean = {
    // TODO: Implement something based on either a spell type or a spell target (allies and or enemies)
    val healingSpells = allSpells.filter(_.isInstanceOf[HealingSpell[_]])
    if (healingSpells.length == 0) return false

    val tempResult = graph.aggregateMessages[(Int, Float)](
      edge => {
        val isEnemy = edge.toEdgeTriplet.attr == 0

        // See NOTE findWeakestEnemy
        if ((edge.srcId == id) && !isEnemy) {
          val key = edge.dstAttr
          val creature = store.value.get(key)

          if (creature.isAlive()) {
            val healRatio = creature.getHealthP()

            // TODO: Either ask the creature or set a threshold
            if (healRatio < 0.33f) {
              edge.sendToSrc((key, healRatio))
            }
          }
        }
      },
      // min health ratio
      (a, b)  => if (b._2 > a._2) a else b)

    val resultAggregate = tempResult.collect()
    if (resultAggregate.length == 0) return false

    val onlyMonoForNow = healingSpells.filter(_.isInstanceOf[MonoHealingSpell])
    val target = store.value.get(resultAggregate(0)._2._1)
    // TODO: Get the most useful
    val spell = onlyMonoForNow(0).asInstanceOf[MonoHealingSpell]
    spell.apply(this, target, null)

    allSpells = allSpells.slice(0, allSpells.length - 1)

    return true
  }

  def regenerate(): Unit = {
    if ((health != maxHealth) && (regeneration != 0)) {
      println(s"\t${Console.GREEN}${name} regenerates ${regeneration} hp.${Console.RESET}")
      heal(regeneration)
    }
  }

  def getHealthP(): Float = {
    val result = health / maxHealth
    return result
  }

  // Ask enemies what their life is. Attack the one with the lowest health.
  protected def findWeakestEnemy(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type])() : Creature = {
    val tempResult = graph.aggregateMessages[(Int, Int)](
      edge => {
        val isEnemy = edge.toEdgeTriplet.attr == 0

        // NOTE: We could check for ((edge.srcId == id) || (edge.dstId == id))
        // if we use a directed graph representation (we which do), with one edge between each vertex
        // (currently, there are two (one for each direction because we want an undirected graph)).
        if ((edge.srcId == id) && isEnemy) {
          val key = edge.dstAttr
          val creature = store.value.get(key)

          if (creature.isAlive()) {
            edge.sendToSrc((key, creature.health))
          }
        }
      },
      // min health
      (a, b)  => if (b._2 > a._2) a else b)

    val resultAggregate = tempResult.collect()

    if (resultAggregate.length == 0) {
      return null
    }

    val result = resultAggregate(0)._2
    return store.value.get(result._1)
  }

  protected def findRandomEnemy(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type])() : Creature = {
    val tempResult = graph.aggregateMessages[Int](
      edge => {
        val isEnemy = edge.toEdgeTriplet.attr == 0

        if ((edge.srcId == id) && isEnemy) {
          val key = edge.dstAttr
          val creature = store.value.get(key)

          if (creature.isAlive()) {
            edge.sendToSrc(key)
          }
        }
      },

      (a, b)  => if (Dice.d10.roll() <= 5) a else b)

    val resultAggregate = tempResult.collect()

    if (resultAggregate.length == 0) {
      return null
    }

    val result = resultAggregate(0)._2
    return store.value.get(result)
  }

  protected def addSpell(s: Spell[_], count: Int): Unit = {
    allSpells ++= (1 to count).map(_ => s)
  }

  protected def addSpell(s: Spell[_]): Unit = {
    allSpells += s
  }
}

object Bestiary {
  case class Solar() extends Creature("Solar") {
    healthFormula = new Formula(22, Dice.d10, 242)

    initiative = 9
    armor = 44
    regeneration = 15

    damageReduction = 15
    spellReduction = 34

    // TODO: Range attacks
    allAttacks += DancingGreatSword
    allAttacks += SolarSlam

    addSpell(CureLightWounds, 3)
    addSpell(CureModerateWounds, 2)
    addSpell(CureSeriousWounds)
    addSpell(CureCriticalWounds, 3)
  }

  case class Planetar() extends Creature("Planetar") {
    healthFormula = new Formula(17, Dice.d10, 136)

    initiative = 8
    armor = 32
    regeneration = 10

    damageReduction = 10
    spellReduction = 27

    // TODO: Range attacks
    allAttacks += HolyGreatSword
    allAttacks += PlanetarSlam

    addSpell(CureLightWounds, 4)
    addSpell(CureModerateWounds, 2)
    addSpell(CureSeriousWounds, 2)
  }

  case class MovanicDeva() extends Creature("Movanic Deva") {
    healthFormula = new Formula(12, Dice.d10, 60)

    initiative = 7
    armor = 24

    damageReduction = 10
    spellReduction = 21

    // TODO: Ranged attacks
    allAttacks += FlamingGreatSword

    // NOTE: It says "7/day"
    addSpell(CureSeriousWounds, 7)
  }

  case class AstralDeva() extends Creature("Astral Deva") {
    healthFormula = new Formula(15, Dice.d10, 90)

    initiative = 8
    armor = 29

    damageReduction = 10
    spellReduction = 25

    // TODO: Ranged attacks
    allAttacks += DisruptingWarhammer
    allAttacks += AstralSlam

    addSpell(CureSeriousWounds, 7)
  }

  case class GreenGreatWyrmDragon() extends Creature("Green Great Wyrm Dragon") {
    healthFormula = new Formula(27, Dice.d12, 216)

    initiative = 2
    armor = 37

    damageReduction = 20
    spellReduction = 31

    // TODO: Ranged attacks
    allAttacks += DragonBite
    allAttacks += Claw
    allAttacks += Wings
    allAttacks += TailSlap
  }

  abstract class Orc(override val name: String) extends Creature(name) {
    override protected def think(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]): Boolean = {
      val strategy = (c: Creature) => findRandomEnemy(id, graph, store)

      return attack(strategy)
    }
  }

  case class OrcBarbarian() extends Orc("Orc Barbarian") {
    healthFormula = new Formula(4, Dice.d12, 16)

    initiative = 1
    armor = 15

    // TODO: Ranged attacks
    allAttacks += GreatAxe
  }

  case class AngelSlayer() extends Orc("Angel Slayer") {
    healthFormula = new Formula(15, Dice.d10, 25)

    initiative = 7
    armor = 26

    // TODO: Ranged attacks
    allAttacks += DoubleAxe
    allAttacks += DoubleAxe2
  }

  case class WorgRider() extends Orc("Worg Rider") {
    healthFormula = new Formula(2, Dice.d10, 2)

    initiative = 2
    armor = 18

    // TODO: Range attacks
    allAttacks += MWKBattleAxe
  }

  case class Warlord() extends Orc("Warlord") {
    healthFormula = new Formula(13, Dice.d10, 65)

    initiative = 2
    armor = 27

    // TODO: Range attacks
    allAttacks += ViciousFlail
    allAttacks += LionShield
  }

  case class BarbaresOrc() extends Orc("Barbares Orc") {
    healthFormula = new Formula(11, Dice.d12, 65)

    initiative = 4
    armor = 17

    damageReduction = 3

    // TODO: Range attacks
    allAttacks += OrcDoubleAxe
    allAttacks += OrcDoubleAxe2
    allAttacks += OrcBite
  }
}