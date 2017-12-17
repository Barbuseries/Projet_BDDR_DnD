import Bestiary._
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{Edge, Graph}
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer

object Main {
  type World = Graph[Int, Relationship.Value]

  // @HACK to simulate the behaviour until we have 3d movement (we won't, ever)
  var fight = 0
  var round = -1
  var roundFightStarts = 0
  var roundFightIsMelee = 0

  // TODO: To implement invocation spells (Summon 7), there needs to be a way to add vertices to the graph.
  def createGraph(sc: SparkContext, allies : Team, enemies : Team): World = {
    val allies_len = allies.members.length
    val enemies_len = enemies.members.length

    val offset = allies_len

    var vertices = allies.vertices()
    vertices ++= enemies.vertices().map(e => (offset + e._1, e._2))

    val allyEdges = allies.edges()
    val enemyEdges = enemies.edges().map(e => Edge(offset + e.srcId, offset + e.dstId, e.attr))
    // Joins allies and enemies
    val inBetweenEdges = for (i <- 0 until allies_len; j <- 0 until enemies_len) yield Edge(i.toLong, (offset + j).toLong, Relationship.Enemy)


    // As GraphX only uses directional graphs, add edges in the other direction
    var edges   = allyEdges ++ inverseEdges(allyEdges)
    edges ++= enemyEdges ++ inverseEdges(enemyEdges)
    edges ++= inBetweenEdges ++ inverseEdges(inBetweenEdges.to[ArrayBuffer])

    val result = Graph(sc.parallelize(vertices), sc.parallelize(edges), allies.members(0))
    return result
  }

  def inverseEdges[T](edges: ArrayBuffer[Edge[T]]): ArrayBuffer[Edge[T]] = {
    return edges.map(e => {
      // In case the edge a goes from and to the same vertex, no need to invert it.
      if (e.dstId != e.srcId) Edge(e.dstId, e.srcId, e.attr)
      else null
    }).filterNot(_ == null)
  }

  def buildFightOne(allies: Team, enemies: Team): Unit = {
    allies.add(Solar())

    enemies.add(WorgRider(), 9)
    enemies.add(Warlord())
    enemies.add(BarbaresOrc(), 4)

    roundFightStarts = 0
    roundFightIsMelee = 3
    fight = 0
  }

  def buildFightTwo(allies: Team, enemies: Team): Unit = {
    allies.add(Solar())
    allies.add(Planetar(), 2)
    allies.add(MovanicDeva(), 2)
    allies.add(AstralDeva(), 5)

    enemies.add(GreenGreatWyrmDragon())
    enemies.add(OrcBarbarian(), 200)
    enemies.add(AngelSlayer(), 10)

    roundFightStarts = 3
    roundFightIsMelee = 7
    fight = 1
  }

  def gameLoop(graph: World, store: Broadcast[CreatureStore.type]): Int = {
    // For each creature, roll initiative (also return the creature key) and sort by smallest one (-initiative).
    // (After the collect, each vertex is of the form (id, (c, ini)).
    // For every mapped vertex (id, (c, ini)) only keep (id, c).
    // NOTE: In practice, id and c are the same.
    var orderList = graph.mapVertices((id, c) => (c, store.value.get(c).initiative + Dice.d20.roll())).vertices
                         .sortBy(-_._2._2)
                         .map(v => (v._1, v._2._1)).collect()

    var done = false
    var winners = 0
    while (!done) {
      round += 1

      orderList.foreach((i) => {
        val id = i._1
        val key = i._2

        var c = store.value.get(key)

        // In case it was killed during this round
        if (c.isAlive()) {
          c.play(id, graph, store)
        }

        winners = getVictoriousTeam(graph, store)
        done = (winners != 0)

        if (done) return winners
      })

      // Remove dead creatures
      orderList = orderList.filter((i) => store.value.get(i._2).isAlive())
    }

    return winners
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

    val winners = gameLoop(graph, store)

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
  private def getVictoriousTeam(graph: World, store: Broadcast[CreatureStore.type]): Int = {
    val teamAliveCount = graph.aggregateMessages[(Int, Int)](
      edge => {
        if (edge.srcId == 0) {
          val creature = store.value.get(edge.dstAttr)

          if (creature.isAlive()) {
            if (edge.attr == Relationship.Ally) {
              edge.sendToSrc((1, 0))
            }
            else {
              edge.sendToSrc((0, 1))
            }
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
      throw new Exception("An ancient multi-dimensional multicolor dragon has appeared! Run for your life!")
    }
  }
}