import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

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

  def play(id: VertexId, graph: Graph[Creature, Int]) : Unit = {
    println(s"$name is playing...")
    // TODO (way later): Ask allies if they need anything

    // TODO: Ask enemies what their life is. Attack the one with the lowest health

    val result = findWeakestEnemy(id, graph)

    if (result._2 != null) {
      attack(result._2)
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
        a.hit(creature)

        return
      }
    }
  }

  private def findWeakestEnemy(id: VertexId, graph: Graph[Creature, Int]) : (VertexId, Creature) = {
    val tempResult = graph.aggregateMessages[(VertexId, Creature)](
      edge => {
        val isEnemy = edge.toEdgeTriplet.attr == 0

        if ((edge.srcId == id) && isEnemy) {
          val creature = edge.dstAttr

          if (creature.isAlive()) {
            edge.sendToSrc((edge.dstId, edge.dstAttr))
          }
        }
      },
      (a, b)  => if (b._2.health > a._2.health) a else b)

    val result = tempResult.collect()

    if (result.length == 0) {
      return (-1, null)
    }

    return result(0)._2
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