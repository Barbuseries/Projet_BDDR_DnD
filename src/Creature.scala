import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import Main.World
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{Edge, EdgeContext, Graph, VertexId}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object Relationship extends Enumeration {
  val Ally, Enemy, All = Value
}

object CreatureType extends Enumeration {
  val Angel, Orc, Dragon = Value
}

abstract class Creature(val name : String) extends Serializable {
  type Message[T] = EdgeContext[Int, Relationship.Value, T]
  type Mapper[T] = (Message[T], Creature, Int) => Unit
  type Reducer[T] = (T, T) => T

  // We only ever send to src, so we do not need an Array
  class Result[T](val value: T) extends Serializable {
  }

  class Context(val id: VertexId, val graph: World, val store: Broadcast[CreatureStore.type]) extends Serializable {
    // TODO: As the creature is fetched anyway (to know if it is alive, pass it as parameter to the map function)
    def onLinked[T: ClassTag](recipient: (Message[T]) => Boolean,
                                      map: Mapper[T],
                                      reduce: Reducer[T]): Result[T] = {
      val result = graph.aggregateMessages[T](
        edge => {
          // NOTE: We could check for ((edge.srcId == id) || (edge.dstId == id))
          // if we use a directed graph representation (we which do), with one edge between each vertex
          // (currently, there are two (one for each direction because we want an undirected graph)).
          val linkedToMe = (edge.srcId == id)
          val key = edge.dstAttr

          if (linkedToMe) {
            if (recipient(edge)) {
              val creature = store.value.get(key)

              if (creature.isAlive()) {
                map(edge, creature, key)
              }
            }
          }
        },
        (a, b) => reduce(a, b)).collect()

      if (result.length == 0) return null

      return new Result(result(0)._2)
    }

    def onAllies[T: ClassTag](map: Mapper[T], reduce: Reducer[T]): Result[T] =
      onLinked[T]((e) => e.attr == Relationship.Ally, map, reduce)

    def onEnemies[T: ClassTag](map: Mapper[T], reduce: Reducer[T]): Result[T] =
      onLinked[T]((e) => e.attr == Relationship.Enemy, map, reduce)

    def onAll[T: ClassTag](map: Mapper[T], reduce: Reducer[T]): Result[T] =
      onLinked[T]((e) => true, map, reduce)
  }

  var creatureType: CreatureType.Value = _

  var initiative: Int = 0

  var healthFormula: Formula = _
  var maxHealth: Int = 0
  var health: Int = 0

  var armor: Int = 0

  // TODO: Look at what breaks damageReduction
  var damageReduction: Int = 0
  var spellReduction: Int = 0

  // TODO: This is only set for the solar (and maybe other angels),
  // does it really belong here?
  var regeneration: Int = 0

  // NOTE: This is actually only useful when doing Angel Slayer -> Angel attack.
  var typeBonusOnAttack: List[(CreatureType.Value, Int)] = List()

  var allAttacks: List[Attack] = List()
  // NOTE: spells are removed after they have been use
  var allSpells: ArrayBuffer[Spell[_]] = ArrayBuffer.empty[Spell[_]]

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

  protected def think(context: Context): Boolean = {
    if(healAllies(context)) return true

    // TODO: Change to use a custom strength evaluation function
    val strategy = (previousTarget : Creature) => {
      if (previousTarget != null)
        previousTarget
      else
        findWeakestEnemy(context)
    }

    if (attack(strategy)) return true

    return false
  }

  def play(id: VertexId, graph: World, store: Broadcast[CreatureStore.type]) : Unit = {
    println(s"$name ($health) is playing...")
    var played = false

    regenerate()

    val context = new Context(id, graph, store)

    played = think(context)

    if (!played) {
      println("\tBut can not do anything...")
    }
  }

  def isAlive(): Boolean = {
    return health > 0
  }

  def attackBonusAgainst(creatureType: CreatureType.Value): Int = {
    val result = typeBonusOnAttack.find(_._1 == creatureType)

    if (!result.isEmpty) return result.head._2
    else return 0
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

  def attack(targetSelector: (Creature) => Creature): Boolean = {
    val firstCreature = targetSelector(null)
    if (firstCreature == null) return false

    val validAttacks = allAttacks.filter(_.canApply(this, firstCreature))
    if (validAttacks.length == 0) return false

    // TODO: Can be changed to rank based on min/max/average damages
    val choosenAttack = validAttacks(scala.util.Random.nextInt(validAttacks.length))

    var damages = choosenAttack.apply(this, firstCreature, targetSelector)

    return true
  }

  protected def healAllies(context: Context): Boolean = {
    // TODO: Implement something based on either a spell type or a spell target (allies and or enemies)
    val healingSpells = allSpells.filter(_.isInstanceOf[HealingSpell[_]])
    if (healingSpells.length == 0) return false

    val tempResult = context.onAllies[(Int, Float)](
      (e, creature, key) => {
          val healRatio = creature.getHealthP()

          // TODO: Either ask the creature or set a threshold
          if (healRatio < 0.33f) {
            e.sendToSrc((key, healRatio))
          }
        },
      // min health ratio
      (a, b)  => if (b._2 > a._2) a else b)

    if (tempResult == null) return false

    val onlyMonoForNow = healingSpells.filter(_.isInstanceOf[MonoHealingSpell])
    val target = context.store.value.get(tempResult.value._1)
    // TODO: Get the most useful
    val spell = onlyMonoForNow(0).asInstanceOf[MonoHealingSpell]
    spell.apply(this, target, null)

    allSpells = allSpells.slice(0, allSpells.length - 1)

    return true
  }

  def regenerate(): Unit = {
    if ((health != maxHealth) && (regeneration != 0)) {
      println(s"\t${Console.GREEN}${name} regenerates ${regeneration} hp.${Console.RESET}")
      heal(regeneration)
    }
  }

  def getHealthP(): Float = {
    val result = (1.0f * health) / maxHealth
    return result
  }

  // Ask entities with the given relationship what their life is. Return the one with the lowest health.
  private def findWeakest(context: Context, relationship: Relationship.Value)(): Creature = {
    var tempResult: Result[(Int, Int)] = null

    val mapper: Mapper[(Int, Int)] = (e, creature, key) => {
      e.sendToSrc((key, creature.health))
    }

    val reducer: Reducer[(Int, Int)] = (a, b) => if (a._2 > b._2) b else a

    if (relationship == Relationship.All)
      tempResult = context.onAll[(Int, Int)](mapper, reducer)
    else
      tempResult = context.onLinked[(Int, Int)]((e) => e.attr == relationship, mapper, reducer)

    if (tempResult == null) return null

    val result = context.store.value.get(tempResult.value._1)

    return result
  }

  private def findWeakestEnemy(context: Context)(): Creature = findWeakest(context, Relationship.Enemy)

  protected def findRandomEnemy(context: Context)() : Creature = {
    val tempResult = context.onEnemies[List[Int]](
      (e, creature, key) => {
          e.sendToSrc(List(key))
      },
      (a, b)  => a ++ b)

    if (tempResult == null) return null

    val enemies = tempResult.value
    val randomIndex = new Dice(enemies.length).roll() - 1

    val result = enemies(randomIndex)
    return context.store.value.get(result)
  }

  protected def addSpell(s: Spell[_], count: Int): Unit = {
    allSpells ++= (1 to count).map(_ => s)
  }

  protected def addSpell(s: Spell[_]): Unit = {
    allSpells += s
  }
}

object Bestiary {
  abstract class Angel(override val name: String) extends Creature(name) {
    creatureType = CreatureType.Angel
  }

  case class Solar() extends Angel("Solar") {
    healthFormula = new Formula(22, Dice.d10, 242)

    initiative = 9
    armor = 44
    regeneration = 15

    damageReduction = 15
    spellReduction = 34

    // TODO: Range attacks
    allAttacks = List(DancingGreatSword, SolarSlam)

    addSpell(CureLightWounds, 3)
    addSpell(CureModerateWounds, 2)
    addSpell(CureSeriousWounds)
    addSpell(CureCriticalWounds, 3)
  }

  case class Planetar() extends Angel("Planetar") {
    healthFormula = new Formula(17, Dice.d10, 136)

    initiative = 8
    armor = 32
    regeneration = 10

    damageReduction = 10
    spellReduction = 27

    // TODO: Range attacks
    allAttacks = List(HolyGreatSword, PlanetarSlam)

    addSpell(CureLightWounds, 4)
    addSpell(CureModerateWounds, 2)
    addSpell(CureSeriousWounds, 2)
  }

  case class MovanicDeva() extends Angel("Movanic Deva") {
    healthFormula = new Formula(12, Dice.d10, 60)

    initiative = 7
    armor = 24

    damageReduction = 10
    spellReduction = 21

    // TODO: Ranged attacks
    allAttacks = List(FlamingGreatSword)

    // NOTE: It says "7/day"
    addSpell(CureSeriousWounds, 7)
  }

  case class AstralDeva() extends Angel("Astral Deva") {
    healthFormula = new Formula(15, Dice.d10, 90)

    initiative = 8
    armor = 29

    damageReduction = 10
    spellReduction = 25

    // TODO: Ranged attacks
    allAttacks = List(DisruptingWarhammer, AstralSlam)

    addSpell(CureSeriousWounds, 7)
  }

  case class GreenGreatWyrmDragon() extends Creature("Green Great Wyrm Dragon") {
    healthFormula = new Formula(27, Dice.d12, 216)

    creatureType = CreatureType.Dragon

    initiative = 2
    armor = 37

    damageReduction = 20
    spellReduction = 31

    // TODO: Ranged attacks
    allAttacks = List(DragonBite, Claw, Wings, TailSlap)
  }

  abstract class Orc(override val name: String) extends Creature(name) {
    creatureType = CreatureType.Orc

    override protected def think(context: Context): Boolean = {
      val strategy = (c: Creature) => findRandomEnemy(context)

      return attack(strategy)
    }
  }

  case class OrcBarbarian() extends Orc("Orc Barbarian") {
    healthFormula = new Formula(4, Dice.d12, 16)

    initiative = 1
    armor = 15

    // TODO: Ranged attacks
    allAttacks = List(GreatAxe)
  }

  case class AngelSlayer() extends Orc("Angel Slayer") {
    healthFormula = new Formula(15, Dice.d10, 25)

    initiative = 7
    armor = 26

    // TODO: Ranged attacks
    allAttacks = List(DoubleAxe, DoubleAxe2)

    typeBonusOnAttack = List((CreatureType.Angel, 8))
  }

  case class WorgRider() extends Orc("Worg Rider") {
    healthFormula = new Formula(2, Dice.d10, 2)

    initiative = 2
    armor = 18

    // TODO: Range attacks
    allAttacks = List(MWKBattleAxe)
  }

  case class Warlord() extends Orc("Warlord") {
    healthFormula = new Formula(13, Dice.d10, 65)

    initiative = 2
    armor = 27

    // TODO: Range attacks
    allAttacks = List(ViciousFlail, LionShield)
  }

  case class BarbaresOrc() extends Orc("Barbares Orc") {
    healthFormula = new Formula(11, Dice.d12, 65)

    initiative = 4
    armor = 17

    damageReduction = 3

    // TODO: Range attacks
    allAttacks = List(OrcDoubleAxe, OrcDoubleAxe2, OrcBite)
  }
}