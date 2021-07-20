package aoc2015

import aoc2015.Day22.GameState.{newPoison, newRecharge, newShield}

import scala.io.Source
import scala.util.Using

object Day22 extends App {

  val input = Using(Source.fromFile("inputs/2015/22.txt"))(_.getLines()
    .map(line => line.split(": "))
    .map(arr => arr(0) -> arr(1).toInt)
    .toMap).get

  abstract sealed class Spell {
    def cost: Int

    def canCast(state: GameState): Boolean = state.wizard.mana >= cost
  }

  case object MagicMissile extends Spell {
    override def cost: Int = 53
  }

  case object Drain extends Spell {
    override def cost: Int = 73
  }

  case class Shield(charges: Int = 6) extends Spell {
    def tick: Shield = copy(charges = charges - 1)

    override def cost: Int = 113

    override def canCast(state: GameState): Boolean = super.canCast(state) && state.shield.charges <= 0
  }

  case class Poison(charges: Int = 6) extends Spell {
    def tick: Poison = copy(charges = charges - 1)

    override def cost: Int = 173

    override def canCast(state: GameState): Boolean = super.canCast(state) && state.poison.charges <= 0
  }

  case class Recharge(charges: Int = 5) extends Spell {
    def tick: Recharge = copy(charges = charges - 1)

    override def cost: Int = 229

    override def canCast(state: GameState): Boolean = super.canCast(state) && state.recharge.charges <= 0
  }

  case class Boss(hp: Int, dmg: Int)

  case class Wizard(hp: Int, mana: Int, armor: Int)

  abstract sealed class GameAction

  case object NoAction extends GameAction

  case object BossAttacks extends GameAction

  case class CastSpell(spell: Spell) extends GameAction

  case class GameState(wizard: Wizard, boss: Boss, isPlayersTurn: Boolean, manaSpent: Int, shield: Shield, poison: Poison, recharge: Recharge) {

    private def mapWizard(f: Wizard => Wizard) = copy(wizard = f(wizard))

    private def mapBoss(f: Boss => Boss) = copy(boss = f(boss))

    def possibleActions: List[GameAction] = {
      if (isFinal) List(NoAction)
      else if (!isPlayersTurn) List(BossAttacks)
      else List(MagicMissile, Drain, newShield, newPoison, newRecharge)
        .filter(_.canCast(this))
        .map(CastSpell)
    }

    private def defend(): GameState = {
      val damageTaken = math.max(1, boss.dmg - wizard.armor)
      mapWizard(w => w.copy(hp = w.hp - damageTaken))
    }

    private def tick(): GameState = copy(
      shield = shield.tick,
      poison = poison.tick,
      recharge = recharge.tick,
    )

    def beginningOfTurn(): GameState = this
      .mapWizard(w => if (recharge.charges > 0) w.copy(mana = w.mana + 101) else w)
      .mapWizard(w => w.copy(armor = if (shield.charges > 0) 7 else 0))
      .mapBoss(b => if (poison.charges > 0) b.copy(hp = b.hp - 3) else b)
      .tick()

    private def penalty(pen: Int): GameState = copy(
      wizard = wizard.copy(hp = if (isPlayersTurn) wizard.hp - pen else wizard.hp)
    )

    def isFinal: Boolean = wizard.hp <= 0 || boss.hp <= 0

    def winner: String = {
      if (wizard.hp <= 0 && boss.hp <= 0) throw new IllegalStateException("both died")
      if (wizard.hp <= 0) "boss" else "wizard"
    }

    def setupTurn(pen: Int): GameState = {
      val a = penalty(pen)
      if (a.isFinal) a else a.beginningOfTurn()
    }

    def expendMana(mana: Int): GameState = {
      if (mana > wizard.mana) throw new IllegalArgumentException
      copy(
        wizard = wizard.copy(mana = wizard.mana - mana),
        manaSpent = manaSpent + mana,
      )
    }

    def turn(action: GameAction): GameState = (action match {
      case NoAction => this
      case BossAttacks => defend()
      case CastSpell(m@MagicMissile) => expendMana(m.cost).mapBoss(b => b.copy(hp = b.hp - 4))
      case CastSpell(d@Drain) => expendMana(d.cost)
        .mapWizard(w => w.copy(hp = w.hp + 2))
        .mapBoss(b => b.copy(hp = b.hp - 2))
      case CastSpell(s@Shield(_)) => expendMana(s.cost).copy(shield = s)
      case CastSpell(p@Poison(_)) => expendMana(p.cost).copy(poison = p)
      case CastSpell(r@Recharge(_)) => expendMana(r.cost).copy(recharge = r)
    }).copy(isPlayersTurn = !isPlayersTurn)
  }

  object GameState {
    private val newShield = Shield()
    private val newPoison = Poison()
    private val newRecharge = Recharge()
  }

  val boss = Boss(input("Hit Points"), input("Damage"))
  val wizard = Wizard(50, 500, 0)
  val state = GameState(wizard, boss, isPlayersTurn = true, manaSpent = 0,
    Shield(0), Poison(0), Recharge(0))

  def wrapper(state: GameState, penalty: Int): Int = {

    var bestSoFar = Int.MaxValue // TODO var :(

    def leastManaSpent(state: GameState): Int = {
      if (state.isFinal && state.winner == "wizard") {
        bestSoFar = math.min(bestSoFar, state.manaSpent)
        state.manaSpent
      }
      else if (state.manaSpent > bestSoFar || state.isFinal && state.winner == "boss") Int.MaxValue
      else {
        val before = state.setupTurn(penalty)
        before.possibleActions.map(action => before.turn(action)).foldLeft(Int.MaxValue) {
          case (acc, state) => math.min(acc, leastManaSpent(state))
        }
      }
    }

    leastManaSpent(state)
  }

  println(wrapper(state, 0))
  println(wrapper(state, 1))
}
