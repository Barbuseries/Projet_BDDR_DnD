import org.apache.spark.graphx.{Edge, VertexId}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Team () {
  var members: ArrayBuffer[Int] = ArrayBuffer.empty[Int]

  def add(creatureTemplate: Creature, count : Int = 1): Unit = {
    creatureTemplate.init()

    println(creatureTemplate.name)
    println("initiative", creatureTemplate.initiative)
    println("health", creatureTemplate.health)
    println("armor", creatureTemplate.armor)

    for (i <- 0 until count - 1) {
      var creature: Creature = creatureTemplate.bulldozerCopy()

      members += CreatureStore.register(creature)
    }

    members += CreatureStore.register(creatureTemplate)
  }

  def vertices(): ArrayBuffer[(VertexId, Int)] = {
    val result = members.zipWithIndex.map{case (c, i) => (i.toLong, c)}

    return result
  }

  // TODO: Store more information inside the edges (last Int field).
  def edges(): ArrayBuffer[Edge[Int]] = {
    val result = for (i <- 0 until members.length; j <- (i + 1) until members.length) yield Edge(i.toLong, j.toLong, 1)

    return result.to[mutable.ArrayBuffer]
  }
}
