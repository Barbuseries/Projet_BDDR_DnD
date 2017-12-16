import Bestiary._
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{Edge, Graph}
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer

object Main {
  // TODO: To implement invocation spells (Summon 7), there needs to be a way to add vertices to the graph.
  def createGraph(sc: SparkContext, allies : Team, enemies : Team): Graph[Int, Int] = {
    val allies_len = allies.members.length
    val enemies_len = enemies.members.length

    val offset = allies_len

    var vertices = allies.vertices()
    vertices ++= enemies.vertices().map(e => (offset + e._1, e._2))

    val allyEdges = allies.edges()
    val enemyEdges = enemies.edges().map(e => Edge(offset + e.srcId, offset + e.dstId, e.attr))
    // Joins allies and enemies
    val inBetweenEdges = for (i <- 0 until allies_len; j <- 0 until enemies_len) yield Edge(i.toLong, (offset + j).toLong, 0)


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

  def buildFightOne(allies: Team, enemies: Team): Unit = {
    allies.add(Solar())

    enemies.add(WorgRider(), 9)
    enemies.add(Warlord())
    enemies.add(BarbaresOrc(), 4)
  }

  def buildFightTwo(allies: Team, enemies: Team): Unit = {
    allies.add(Solar())
    allies.add(Planetar(), 2)
    allies.add(MovanicDeva(), 2)
    allies.add(AstralDeva(), 5)

    enemies.add(GreenGreatWyrmDragon())
    enemies.add(OrcBarbarian(), 200)
    enemies.add(AngelSlayer(), 10)
  }

  def main(args: Array[String]) {
    val conf = new SparkConf()
      .setAppName("toto")
      .setMaster("local[4]")

    val sc = new SparkContext(conf)
    sc.setLogLevel("ERROR")

    var store = sc.broadcast(CreatureStore)

    var allies = new Team()
    var enemies = new Team()

    //buildFightOne(allies, enemies)
    buildFightTwo(allies, enemies)

    val graph = createGraph(sc, allies, enemies)

    val orderList = graph.mapVertices((id, c) => store.value.get(c).initiative).vertices.collect().sortBy(-_._2).map(_._1)

    var done = false
    var winners = 0
    while (!done) {
      // FIXME: This is used to iterate over all elements in order (without having to keep an index around).
      //  There may (should) be a better way to do this (having to filter is bad), but I don't know it yet.
      orderList.foreach(id => {
        var key = graph.vertices.filter(_._1 == id).first()._2
        var c = store.value.get(key)

        if (c.isAlive()) {
          c.play(id, graph, store)
        }
      }
    )

      // FIXME?: Btw, as the check is done after every creature has played,
      // some may spend their time doing nothing or healing allies when
      // their enemies are already defeated.
      winners = getVictoriousTeam(graph, store)
      done = (winners != 0)
    }

    if (winners == 0) {
      println("How did _that_ happen?")
    }
    else if (winners > 0) {
      println(s"\t\t\t\tALLIES WIN. ${Console.BLINK}${Console.RED}DIVINITY.${Console.RESET}")
      println(s"${winners} allies left.")
    }
    else {
      println(s"\t\t\t\t${Console.RED_B}GAME OVER${Console.RESET}")
      println(s"${-winners} enemies left.")
    }
  }

  // > 0 = allies (count)
  // < 0 = enemies (-count)
  // 0 = none
  private def getVictoriousTeam(graph: Graph[Int, Int], store: Broadcast[CreatureStore.type]): Int = {
    var askedFirstVertex = false
    val teamAliveCount = graph.aggregateMessages[(Int, Int)](
      edge => {
        if (edge.srcId == 0) {
          val creature = store.value.get(edge.dstAttr)

          if (creature.isAlive()) {
            if (edge.attr == 1) {
              edge.sendToSrc((1, 0))
            }
            else {
              edge.sendToSrc((0, 1))
            }
          }

          // Get the first node once as well
          if (!askedFirstVertex) {
            val creature = store.value.get(edge.srcAttr)
            if (creature.isAlive()) {
              edge.sendToSrc((1, 0))
            }
            else { // So we at least always have a value. Just in case...
              edge.sendToSrc((0, 0))
            }

            askedFirstVertex = true
          }
        }
      },
      (a, b) => (a._1 + b._1, a._2 + b._2)
    ).collect().head

    val allyCount = teamAliveCount._2._1
    val enemyCount = teamAliveCount._2._2

    val alliesAlive  = (allyCount != 0)
    val enemiesAlive = (enemyCount != 0)

    if (alliesAlive && enemiesAlive) {
      return 0
    }
    else if (alliesAlive) {
      return allyCount
    }
    else if (enemiesAlive) {
      return -enemyCount
    }
    else {
      throw new Exception("A ancient multi-dimensional multicolor dragon has appeared! Run for your life!")
    }
  }
}