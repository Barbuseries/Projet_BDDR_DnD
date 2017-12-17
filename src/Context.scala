import Context.{Mapper, Message, Reducer}
import Main.World
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{EdgeContext, VertexId}

import scala.reflect.ClassTag

// We only ever send to src, so we do not need an Array
class Result[T](val value: T) extends Serializable {
}

object Context {
  type Message[T] = EdgeContext[Int, Relationship.Value, T]
  type Mapper[T] = (Message[T], Creature, Int) => Unit
  type Reducer[T] = (T, T) => T
}

class Context(val id: VertexId, val graph: World, val store: Broadcast[CreatureStore.type]) extends Serializable {
  def onLinked[T: ClassTag](recipient: (Message[T]) => Boolean,
                            map: Mapper[T],
                            reduce: Reducer[T],
                            includeSelf: Boolean = false): Result[T] = {
    val result = graph.aggregateMessages[T](
      edge => {
        // NOTE: We could check for ((edge.srcId == id) || (edge.dstId == id))
        // if we use a directed graph representation (we which do), with one edge between each vertex
        // (currently, there are two (one for each direction because we want an undirected graph)).
        val linkedToMe = (edge.srcId == id)
        val key = edge.dstAttr

        if (linkedToMe) {
          val isMe = (edge.dstId == id)

          if (!isMe || (isMe && includeSelf)) {
            if (recipient(edge)) {
              val creature = store.value.get(key)

              if (creature.isAlive()) {
                map(edge, creature, key)
              }
            }
          }
        }
      },
      (a, b) => reduce(a, b)).collect()

    if (result.length == 0) return null

    return new Result(result(0)._2)
  }

  def onAllies[T: ClassTag](map: Mapper[T], reduce: Reducer[T], includeSelf: Boolean = false): Result[T] =
    onLinked[T]((e) => e.attr == Relationship.Ally, map, reduce, includeSelf)

  def onEnemies[T: ClassTag](map: Mapper[T], reduce: Reducer[T]): Result[T] =
    onLinked[T]((e) => e.attr == Relationship.Enemy, map, reduce, false)

  def onAll[T: ClassTag](map: Mapper[T], reduce: Reducer[T], includeSelf: Boolean = false): Result[T] =
    onLinked[T]((e) => true, map, reduce, includeSelf)
}