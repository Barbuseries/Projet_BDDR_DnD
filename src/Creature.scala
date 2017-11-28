import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{Graph, VertexId}
import org.jsoup.{HttpStatusException, Jsoup}
import org.jsoup.nodes.Document

import scala.collection.mutable.ArrayBuffer

case class Creature(val name : String) extends Serializable {
  var initiative: Int = 0
  var health: Int = 0

  var armor: Int = 0

  var allAttacks: ArrayBuffer[Attack] = ArrayBuffer.empty[Attack]

  def loadFromUrl(url : String): Unit = {
    var doc = getDoc(url)

    initiative = getInitiative(doc)
    health = getHealth(doc)
    armor = getArmor(doc)
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
    println(s"$name is playing...")
    // TODO (way later): Ask allies if they need anything

    // TODO: Ask enemies what their life is. Attack the one with the lowest health

    val result = findWeakestEnemy(id, graph, store)

    if (result._2 != -1) {
      attack(store.value.get(result._2))
    }
  }

  def isAlive(): Boolean = {
    return health > 0
  }

  def takeDamages(damages: Int): Unit = {
    health -= damages

    if (health < 0) health = 0
  }

  def attack(creature: Creature): Unit = {
    for (a <- allAttacks) {
      if (a.canHit(creature)) {
        println(s"\t$name attacks ${creature.name}!")
        a.hit(creature)

        return
      }
    }
  }

  private def findWeakestEnemy(id: VertexId, graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]) : (VertexId, Int) = {
    val tempResult = graph.aggregateMessages[(VertexId, Int, Int)](
      edge => {
        val isEnemy = edge.toEdgeTriplet.attr == 0

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
}