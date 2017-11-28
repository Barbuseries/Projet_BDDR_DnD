import org.apache.spark.graphx.{Edge, Graph, VertexId}
import org.apache.spark.{SparkConf, SparkContext, graphx}

import scala.collection.mutable.ArrayBuffer

object Main {
  def createGraph(sc: SparkContext, allies : Team, enemies : Team): Graph[Creature, Int] = {
    val allies_len = allies.members.length
    val enemies_len = enemies.members.length

    val offset = allies_len

    var vertices = allies.vertices()
    vertices ++= enemies.vertices().map(e => (offset + e._1, e._2))

    val allyEdges = allies.edges()
    val enemyEdges = enemies.edges().map(e => Edge(offset + e.srcId, offset + e.dstId, e.attr))
    // Joins allies and enemies
    val inBetweenEdges = (for (i <- 0 until allies_len; j <- 0 until enemies_len) yield Edge(i.toLong, (offset + j).toLong, 0))


    // As GraphX only uses directional graphs, add edges in the other direction
    var edges   = allyEdges ++ inverseEdges(allyEdges)
    edges ++= enemyEdges ++ inverseEdges(enemyEdges)
    edges ++= inBetweenEdges ++ inverseEdges(inBetweenEdges.to[ArrayBuffer])

    val result = Graph(sc.parallelize(vertices), sc.parallelize(edges), allies.members(0))
    return result
  }

  def inverseEdges(edges: ArrayBuffer[Edge[Int]]): ArrayBuffer[Edge[Int]] = {
    return edges.map(e => Edge(e.dstId, e.srcId, e.attr))
  }

  def main(args: Array[String]) {
    val conf = new SparkConf()
      .setAppName("toto")
      .setMaster("local[1]")

    val sc = new SparkContext(conf)
    sc.setLogLevel("ERROR")

    var allies = new Team()
    allies.add("Solar", "http://www.d20pfsrd.com/bestiary/monster-listings/outsiders/angel/solar")

    var enemies = new Team()
    enemies.add("Worg Rider", "http://www.d20pfsrd.com/bestiary/npc-s/npc-1/orc-worg-rider/", 9)
    enemies.add("Warlord", "http://www.d20pfsrd.com/bestiary/npc-s/npc-12/brutal-warlord-half-orc-fighter-13/")
    enemies.add("Barbares Orc", "http://www.d20pfsrd.com/bestiary/npc-s/npc-10/double-axe-fury-half-orc-barbarian-11/", 4)

    val graph = createGraph(sc, allies, enemies)

    val orderList = graph.mapVertices((id, c) => c.initiative).vertices.collect().sortBy(-_._2).map(_._1)

    var i = 0;
    var done = false;
    while (!done && i == 0) {
      // TODO: Game logic
      // FIXME: This is used to iterate over all elements in order (without having to keep an index around).
      //  There may (should) be a better way to do this (having to filter is bad), but I don't know it yet.
      // FIXME: It seems that changing any element in the graph creates a new graph. And we currently do not store it!
      //  This means that the current structure is useless...
      orderList.map(id => {
        var c = graph.vertices.filter(_._1 == id).first()._2

        if ((c.isAlive()) && (i == 0)) {
          c.play(id, graph)
          //i = 1
        }
      }
    )

      done = onlyAlliesOrEnemies(orderList, allies.members.length)
    }
  }

  // TODO/FIXME: This assumes orderList is modified to only keep alive creatures. This will probably not be the case.
  // FIX THIS!
  private def onlyAlliesOrEnemies(ids: Array[graphx.VertexId], offset : Int): Boolean = {
    return ids.filter(_ < offset).length == ids.length
  }
}