import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{Graph, VertexId}
import org.jsoup.{HttpStatusException, Jsoup}
import org.jsoup.nodes.Document

import scala.collection.mutable.ArrayBuffer

abstract class Creature(val name : String, val url: String) extends Serializable {
  var initiative: Int = 0
  // TODO: Add max hp
  var health: Int = 0

  var armor: Int = 0

  var allAttacks: ArrayBuffer[Attack] = ArrayBuffer.empty[Attack]

  def init(): Unit = {
    loadFromUrl(url)
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

  def takeDamages(damages: Int): Unit = {
    health -= damages

    if (health < 0) health = 0
  }

  def attack(creature: Creature): Boolean = {
    val validAttacks = allAttacks.filter(_.canHit(creature))

    if (validAttacks.length == 0) return false

    // TODO: Can be changed to rank based on min/max/average damages
    val choosenAttack = validAttacks(scala.util.Random.nextInt(validAttacks.length))

    var damages = choosenAttack.hit(this, creature)

    return true
  }

  private def findWeakestEnemy(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]) : (VertexId, Int) = {
    val tempResult = graph.aggregateMessages[(VertexId, Int, Int)](
      edge => {
        val isEnemy = edge.toEdgeTriplet.attr == 0

        // NOTE: We could check for ((edge.srcId == id) || (edge.dstId == id))
        // if we use a directed graph representation (we which do), with one edge between each vertex
        // (currently, there are two (one for each direction because we want an undirected graph).
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

  private def getStat(doc: Document, stat: String, p : String = ""): Int = {
    var element = doc.select(s"p:contains($stat )").first().text()

    val pattern = raw""".*$stat $p(\d+).*""".r
    val pattern(result) = element

    return result.toInt
  }

  private def getInitiative(doc: Document): Int = {
    var result = getStat(doc, "Init", raw"\+")
    return result
  }

  private def getHealth(doc: Document): Int = {
    var result = getStat(doc, "hp")
    return result
  }

  private def getArmor(doc: Document): Int = {
    var result = getStat(doc, "AC")
    return result
  }

  private def getDoc(url: String): Document = {
    try {
      val doc = Jsoup.connect(url).get()

      return doc
    }
    catch {
      case hse: HttpStatusException => { println(s"invalid url: $url"); return null }
      case e: Exception => { println(e); return null }
    }
  }

  protected def loadFromUrl(url : String): Unit = {
    var doc = getDoc(url)

    initiative = getInitiative(doc)
    health = getHealth(doc)
    armor = getArmor(doc)
  }
}

object Bestiary {
  case class Solar() extends Creature("Solar",
    "http://www.d20pfsrd.com/bestiary/monster-listings/outsiders/angel/solar") {
    // TODO: Range attacks
    allAttacks += DancingGreatSword
    allAttacks += Slam
  }

  case class WorgRider() extends Creature("Worg Rider",
    "http://www.d20pfsrd.com/bestiary/npc-s/npc-1/orc-worg-rider/") {
    // TODO: Range attacks
    allAttacks += MWKBattleAxe
  }

  case class Warlord() extends Creature("Warlord",
    "http://www.d20pfsrd.com/bestiary/npc-s/npc-12/brutal-warlord-half-orc-fighter-13/") {

    // TODO: Range attacks
    allAttacks += ViciousFlail
    allAttacks += LionShield
  }

  case class BarbaresOrc() extends Creature("Barbares Orc",
    "http://www.d20pfsrd.com/bestiary/npc-s/npc-10/double-axe-fury-half-orc-barbarian-11/") {
    // TODO: Range attacks

    allAttacks += OrcDoubleAxe
    allAttacks += OrcDoubleAxe2
    allAttacks += Bite
  }
}