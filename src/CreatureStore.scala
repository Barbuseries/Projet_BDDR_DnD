import scala.collection.mutable.ArrayBuffer

// TODO/NOTE: This is intented to store each creature separately
// from the graph itself (because the latter is immutable).
//
// Therefore, each vertex of the graph only has a key into this storage
// from which we can get (and modify) the associated creature.
//
// The other important point is for register to just take a creature
// (the graph is created later, so we have no info on that).
object CreatureStore extends Serializable {
  private var storage: ArrayBuffer[Creature] = ArrayBuffer.empty[Creature]

  def get(key: Int): Creature = {
    return storage(key)
  }

  def register(creature: Creature): Int = {
    storage += creature;

    return storage.length - 1;
  }
}
