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

  // TODO: This is only set for the solar (and maybe other angels),
  // does it really belong here?
  var regeneration: Int = 0

  var allAttacks: ArrayBuffer[Attack] = ArrayBuffer.empty[Attack]

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

  def play(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]) : Unit = {
    println(s"$name ($health) is playing...")
    var played = false

    regenerate()

    // TODO (way later): Ask allies if they need anything

    // TODO: Change to use a custom strength evaluation function
    // Ask enemies what their life is. Attack the one with the lowest health
    val result = findWeakestEnemy(id, graph, store)

    if (result._2 != -1) {
      played = attack(store.value.get(result._2))
    }



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

  def attack(creature: Creature): Boolean = {
    val validAttacks = allAttacks.filter(_.canHit(this, creature))

    if (validAttacks.length == 0) return false

    // TODO: Can be changed to rank based on min/max/average damages
    val choosenAttack = validAttacks(scala.util.Random.nextInt(validAttacks.length))

    var damages = choosenAttack.hit(this, creature)

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
    return health
  }

  private def findWeakestEnemy(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]) : (VertexId, Int) = {
    val tempResult = graph.aggregateMessages[(VertexId, Int, Int)](
      edge => {
        val isEnemy = edge.toEdgeTriplet.attr == 0

        // NOTE: We could check for ((edge.srcId == id) || (edge.dstId == id))
        // if we use a directed graph representation (we which do), with one edge between each vertex
        // (currently, there are two (one for each direction because we want an undirected graph)).
        if ((edge.srcId == id) && isEnemy) {
          val key = edge.dstAttr
          val creature = store.value.get(key)

          if (creature.isAlive()) {
            edge.sendToSrc((edge.dstId, key, creature.health))
          }
        }
      },
      // min health
      (a, b)  => if (b._3 > a._3) a else b)

    val resultAggregate = tempResult.collect()

    if (resultAggregate.length == 0) {
      return (-1, -1)
    }

    // Return just the vertex id and the creature key
    val result = resultAggregate(0)._2
    return (result._1, result._2)
  }
}

object Bestiary {
  case class Solar() extends Creature("Solar") {
    healthFormula = new Formula(22, Dice.d10, 242)

    initiative = 9
    armor = 44
    regeneration = 15

    // TODO: Range attacks
    allAttacks += DancingGreatSword
    allAttacks += Slam
  }

  case class WorgRider() extends Creature("Worg Rider") {
    healthFormula = new Formula(2, Dice.d10, 2)

    initiative = 2
    armor = 18

    // TODO: Range attacks
    allAttacks += MWKBattleAxe
  }

  case class Warlord() extends Creature("Warlord") {
    healthFormula = new Formula(13, Dice.d10, 65)

    initiative = 2
    armor = 27

    // TODO: Range attacks
    allAttacks += ViciousFlail
    allAttacks += LionShield
  }

  case class BarbaresOrc() extends Creature("Barbares Orc") {
    healthFormula = new Formula(11, Dice.d12, 65)

    initiative = 4
    armor = 17

    // TODO: Range attacks
    allAttacks += OrcDoubleAxe
    allAttacks += OrcDoubleAxe2
    allAttacks += Bite
  }
}