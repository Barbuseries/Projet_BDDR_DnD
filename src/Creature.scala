import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import Context.{Mapper, Reducer}
import Main.World
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.graphx.{VertexId}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Relationship extends Enumeration {
  val Ally, Enemy, All = Value
}

object CreatureType extends Enumeration {
  val Angel, Orc, Dragon, Human = Value
}

abstract class Creature(val name : String) extends Serializable {
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

  var allAttacks: List[List[Action[_]]] = List()
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

    if (attackAccordingTo(strategy)) return true

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

  def takeDamages(amount: Int, attacker: Creature): Unit = {
    // TODO: Special message if we happen to have amount == 0?
    // (This is why there is this assert, in case it _does_
    // happen and I forgot to do it.
    assert(amount > 0)
    health -= amount

    if (health <= 0) {
      health = 0

      println(s"\t${Console.RED}${name} was slain by ${attacker.name}!${Console.RESET}")
    }
  }

  def heal(amount: Int): Unit = {
    // TODO: Special message if we happen to have amount == 0?
    // (This is why there is this assert, in case it _does_
    // happen and I forgot to do it.
    assert(amount > 0)

    health += amount

    if (health > maxHealth) health = maxHealth
  }

  def attack[T](attacks: List[Action[_]], target: T, targetSelector: (T) => T): Unit = {
    var currentTarget = target

    if (targetSelector != null) {
      attacks.foreach((a) => {
        currentTarget = targetSelector(a.asInstanceOf[Action[T]].apply(this, currentTarget, targetSelector))
      })
    }
    else {
      attacks.foreach((a) => {
        currentTarget = a.asInstanceOf[Action[T]].apply(this, currentTarget, null)
      })
    }
  }

  // FIXME: Oh boy...
  // It's late. I should not be doing this.
  def attackAccordingTo[T](targetSelector: (T) => T): Boolean = {
    var firstCreature = targetSelector(null.asInstanceOf[T])
    if (firstCreature == null) return false

    var checkAllValid: (Action[T], T) => Boolean = null

    if (firstCreature.isInstanceOf[List[_]]) {
      val checkAllValid = (a: Action[List[_]], l: List[Creature]) => l.map(a.canApply(this, _)).reduce((a, b) => a && b)
    }
    else {
      checkAllValid = (a: Action[T], c: T) => a.canApply(this, c.asInstanceOf[Creature])
    }

    // Can apply all attacks in the same list
    var validAttacks = allAttacks.filter(_.isInstanceOf[List[Action[T]]]).map(_.asInstanceOf[List[Action[T]]])
    validAttacks = validAttacks.filter(_.map(checkAllValid(_, firstCreature)).reduce((a, b) => a && b))

    if (validAttacks.length == 0) return false

    // TODO: Can be changed to rank based on min/max/average damages
    val choosenAttack = validAttacks(scala.util.Random.nextInt(validAttacks.length))
    attack(choosenAttack, firstCreature, targetSelector)

    return true
  }

  protected def useSpell[T](spell: Spell[T], target: T, targetSelector: (T) => T): Unit = {
    val index = allSpells.indexOf(spell)
    assert(index != -1)
    spell.apply(this, target, targetSelector)

    allSpells.remove(index)
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
    useSpell(spell, target, null)

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
  private def findWeakest(context: Context, relationship: Relationship.Value, validator: (Creature) => Boolean)(): Creature = {
    var tempResult: Result[(Int, Int)] = null

    val mapper: Mapper[(Int, Int)] = (e, creature, key) => {
      if (validator(creature)) e.sendToSrc((key, creature.health))
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

  protected def findWeakestEnemy(context: Context, validator: (Creature) => Boolean = (c) => true)(): Creature =
    findWeakest(context, Relationship.Enemy, validator)

  protected def findRandomEnemies(context: Context, count: Int) : List[Creature] = {
    val tempResult = context.onEnemies[List[Int]](
      (e, creature, key) => {
        e.sendToSrc(List(key))
      },
      (a, b)  => a ++ b)

    if (tempResult == null) return null

    val enemies = Random.shuffle(tempResult.value).take(count)

    val result = enemies.map(context.store.value.get(_))
    return result
  }

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
  // Make own think method
  abstract class Angel(override val name: String) extends Creature(name) {
    creatureType = CreatureType.Angel

    private def findWeakestAngelSlayer(context: Context): Creature = {
        val result = context.onEnemies[(Int, Int)]((e, creature, key) => {
          creature match {
            case slayer: AngelSlayer => e.sendToSrc(key, slayer.health)
            case _ =>
          }
        },
          (a, b) => if (a._2 > b._2) b else a)

      if (result == null) return null
      return context.store.value.get(result.value._1)
    }

    override protected def think(context: Context): Boolean = {
      if ((Main.fight == 1) && (Main.round < Main.roundFightStarts)) {
        Main.round match {
          case 0 => println(s"\t${name} is wary...")
          case _ => println(s"\t${name} sees a frighten villager!")
        }
      }

      if(healAllies(context)) return true

      // TODO: Change to use a custom strength evaluation function
      val strategy = (previousTarget : Creature) => {
        if (previousTarget != null) {
          previousTarget
        }
        else {
          var target = findWeakestAngelSlayer(context)

          if (target == null) {
            // Can not attack humans! (altered dragon)
            // It would not get attacked anyway, because it is only the weakest when alone.
            target = findWeakestEnemy(context, _.creatureType != CreatureType.Human)
          }

          target
        }
      }

      if (attackAccordingTo(strategy)) return true

      return false
    }
  }

  case class Solar() extends Angel("Solar") {
    healthFormula = new Formula(22, Dice.d10, 242)

    initiative = 9
    armor = 44
    regeneration = 15

    damageReduction = 15
    spellReduction = 34

    allAttacks = List(List(DancingGreatSword)/*, SolarSlam*/, List(CompositeLongbow))

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
    allAttacks = List(List(HolyGreatSword)/*, PlanetarSlam*/)

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
    allAttacks = List(List(FlamingGreatSword))

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
    allAttacks = List(List(DisruptingWarhammer)/*, AstralSlam*/)

    addSpell(CureSeriousWounds, 7)
  }

  case class GreenGreatWyrmDragon() extends Creature("Green Great Wyrm Dragon") {
    healthFormula = new Formula(27, Dice.d12, 216)

    creatureType = CreatureType.Dragon

    initiative = 2
    armor = 37

    damageReduction = 20
    spellReduction = 31

    allAttacks = List(List(DragonBite, Claw, Wings, TailSlap), List(AcidBreath))

    addSpell(AlterSelf)

    private def roundWhenCloseEnough(): Int = {
      return Main.roundFightStarts - 1
    }

    private def getSolar(context: Context): Solar = {
      // As there is only one Solar, returning one is not too expensive (I think...)
      var result = context.onEnemies[Solar](
        (e, creature, key) => {
          creature match {
            case solar: Solar => e.sendToSrc(solar)
            case _ =>
          }
        },
          (a, b) => a)

      assert(result != null)

      if (result == null) return null
      return result.value
    }

    private def thinkFirstRound(context: Context): Boolean = {
      useSpell(AlterSelf, this, null)

      return true
    }

    private def thinkWhileHuman(context: Context) : Boolean = {
      val solar: Creature = getSolar(context)

      if (Main.round != roundWhenCloseEnough()) {
        println(s"\t${name} gets closer to ${solar.name}...")
      }
      else {
        println(s"\t${name} changes back to a dragon!")
        creatureType = CreatureType.Dragon

        attack(allAttacks.head, solar, (creature: Creature) => creature)

        println(s"\t${name} takes off!")
      }

      return true
    }

    // TODO: Make it fly and not hittable by melee attacks
    private def thinkRemainingFight(context: Context): Boolean = {
      val acidBreath = allAttacks(1)
      val enemies = findRandomEnemies(context, 3)
      attack(acidBreath, enemies, null)

      return enemies.length != 0
    }

    override protected def think(context: Context): Boolean = {
      val played = Main.round match {
        case 0 => thinkFirstRound(context)
        case x if (x <= roundWhenCloseEnough()) => thinkWhileHuman(context)
        case _ => thinkRemainingFight(context)
      }

      return played
    }
  }

  abstract class Orc(override val name: String) extends Creature(name) {
    creatureType = CreatureType.Orc

    override protected def think(context: Context): Boolean = {
      val strategy = (c: Creature) => findRandomEnemy(context)

      if (Main.fight == 1) {
        val stopHidingRound = Main.roundFightStarts - 1
        if (Main.round < stopHidingRound) {
          println(s"\t${Console.CYAN}${name} hides in a bush...${Console.RESET}")
          return true
        }
        else if (Main.round == stopHidingRound) {
          println(s"\t${Console.CYAN}${name} emerges from its bush!${Console.RESET}")
          return true
        }
        else if (Main.round < Main.roundFightIsMelee) {
          println(s"\t${Console.CYAN}${name} rushes its prey!${Console.RESET}")
        }
      }
      else if (Main.round < Main.roundFightIsMelee) {
        println(s"\t${Console.CYAN}${name} rushes its prey!${Console.RESET}")
      }

      return attackAccordingTo(strategy)
    }
  }

  case class OrcBarbarian() extends Orc("Orc Barbarian") {
    healthFormula = new Formula(4, Dice.d12, 16)

    initiative = 1
    armor = 15

    allAttacks = List(List(GreatAxe), List(OrcThrowingAxe))
  }

  case class AngelSlayer() extends Orc("Angel Slayer") {
    healthFormula = new Formula(15, Dice.d10, 25)

    initiative = 7
    armor = 26

    allAttacks = List(List(DoubleAxe, DoubleAxe2), List(MWKCompositeLongbow2))

    typeBonusOnAttack = List((CreatureType.Angel, 8))
  }

  case class WorgRider() extends Orc("Worg Rider") {
    healthFormula = new Formula(2, Dice.d10, 2)

    initiative = 2
    armor = 18

    allAttacks = List(List(MWKBattleAxe), List(ShortBow))
  }

  case class Warlord() extends Orc("Warlord") {
    healthFormula = new Formula(13, Dice.d10, 65)

    initiative = 2
    armor = 27

    allAttacks = List(List(ViciousFlail, LionShield), List(ViciousFlail2), List(MWKThrowingAxe))
  }

  case class BarbaresOrc() extends Orc("Barbares Orc") {
    healthFormula = new Formula(11, Dice.d12, 65)

    initiative = 4
    armor = 17

    damageReduction = 3

    allAttacks = List(List(OrcDoubleAxe), List(OrcDoubleAxe2, OrcDoubleAxe3, OrcBite), List(MWKCompositeLongbow))
  }
}