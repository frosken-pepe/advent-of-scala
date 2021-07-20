package aoc2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day15 extends App {

  case class Vec2(x: Int, y: Int) {
    def neighs(): Set[Vec2] = (for {
      dx <- -1 to 1
      dy <- -1 to 1 if (dx == 0) ^ (dy == 0)
    } yield Vec2(x + dx, y + dy)).toSet
  }

  object Vec2 {
    implicit val readingOrder: Ordering[Vec2] = (a: Vec2, b: Vec2) =>
      if (a.y == b.y) Integer.compare(a.x, b.x)
      else Integer.compare(a.y, b.y)
  }

  case class Unit(id: String, pos: Vec2, elf: Boolean, hp: Int, ap: Int) {
    def alive(): Boolean = hp > 0
  }

  case class State(open: Set[Vec2], units: Set[Unit], ended: Boolean, roundNo: Int, deadElves: Int) {

    def nextRound(): State = copy(roundNo = roundNo + 1)

    def unitById(unitId: String): Option[Unit] = units.filter(_.id == unitId).find(_.alive())

    def unitsOfType(elf: Boolean): Set[Unit] = units.filter(_.alive()).filter(him => him.elf == elf)

    def removeDead(): State =
      copy(units = units.filter(_.alive()), deadElves = deadElves + units.filter(_.elf).count(e => !e.alive()))

    def unoccupied(square: Vec2): Boolean =
      open.contains(square) && !units.filter(_.alive()).exists(_.pos == square)

    def replaceUnit(oldUnit: Unit, newUnit: Unit): State = copy(units = units.map {
      case u if u.id == oldUnit.id => newUnit
      case u => u
    })

    def withElfAp(ap: Int): State =
      this.copy(units = units.map {
        case u if u.elf => u.copy(ap = ap)
        case u => u
      })
  }

  val init: State = {
    val lines = Using(Source.fromFile("inputs/2018/15.txt"))(_.getLines().toList).get
    val units = lines.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.flatMap {
        case ('G', x) => Some(Unit(s"G/$x/$y", Vec2(x, y), elf = false, 200, 3))
        case ('E', x) => Some(Unit(s"E/$x/$y", Vec2(x, y), elf = true, 200, 3))
        case _ => None
      }
    }
    val open = lines.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.flatMap {
        case ('G', x) => Some(Vec2(x, y))
        case ('E', x) => Some(Vec2(x, y))
        case ('.', x) => Some(Vec2(x, y))
        case _ => None
      }
    }
    State(open.toSet, units.toSet, ended = false, 0, 0)
  }

  def round(stopOnDeadElf: Boolean)(state: State): State = {
    val unitOrder = state.units.toList.sortBy(_.pos).map(_.id)
    val result = unitOrder.foldLeft(state) {
      case (state, unitId) if !state.ended => turn(state, unitId).getOrElse(state)
      case (state, _) => state
    }.removeDead().nextRound()
    if (stopOnDeadElf && result.deadElves > 0) result.copy(ended = true)
    else result
  }

  def turn(state: State, unitId: String): Option[State] = for {
    me <- state.unitById(unitId)
    updatedState <- updateState(state, me, state.unitsOfType(!me.elf))
  } yield updatedState

  def updateState(state: State, me: Unit, targets: Set[Unit]): Option[State] = {
    if (targets.isEmpty) Some(state.copy(ended = true))
    else if (me.pos.neighs().intersect(targets.map(_.pos)).nonEmpty) attack(me, state)
    else attackIfPossible(me, moveMe(me, state))
  }

  def attackIfPossible(me: Unit, moved: Option[State]): Option[State] = (for {
    moved <- moved
    attacker <- moved.unitById(me.id)
    attacked <- attack(attacker, moved)
  } yield attacked).orElse(moved)

  def moveMe(me: Unit, state: State): Option[State] = {
    for {
      move <- findBestMove(me, state, state.unitsOfType(!me.elf))
      newMe = me.copy(pos = move)
    } yield state.replaceUnit(me, newMe)
  }

  def attack(me: Unit, state: State): Option[State] = {
    val targetId = state
      .unitsOfType(!me.elf)
      .filter(him => me.pos.neighs().contains(him.pos))
      .map(u => (u.hp, u.pos, u.id))
      .toList
      .minOption
      .map(_._3)

    for {
      tid <- targetId
      him <- state.unitById(tid)
      attacked = state.replaceUnit(him, him.copy(hp = him.hp - me.ap)).removeDead()
    } yield attacked
  }

  def findBestMove(unit: Unit, state: State, targets: Set[Unit]): Option[Vec2] = {
    val inRange = targets.map(_.pos).flatMap(_.neighs()).filter(state.unoccupied)
    val moves = for {
      move <- unit.pos.neighs() if state.unoccupied(move)
      dist <- shortestDistance(move, inRange, state)
    } yield (dist, move)
    moves.toList.minOption.map(_._2)
  }

  def shortestDistance(begin: Vec2, end: Set[Vec2], state: State): Option[Int] = {
    @tailrec def shortestDistanceTailrec(prev: Set[Vec2], visited: Set[Vec2], dist: Int): Option[Int] = {
      if (prev.isEmpty) None
      else if ((end intersect prev).nonEmpty) Some(dist)
      else {
        val next = for {
          p <- prev
          neigh <- p.neighs()
          if state.unoccupied(neigh)
          if !visited.contains(neigh)
        } yield neigh
        shortestDistanceTailrec(next, visited ++ next, dist + 1)
      }
    }

    shortestDistanceTailrec(Set(begin), Set(), 0)
  }

  def game(stopIfElfDies: Boolean)(state: State): State =
    LazyList.iterate(state)(round(stopIfElfDies)).dropWhile(s => !s.ended).head

  def outcome(state: State): Int = (state.roundNo - 1) * state.units.toList.map(_.hp).sum

  val p1 = outcome(game(stopIfElfDies = false)(init))
  println(p1)
  assert(p1 == 224370)

  val p2 = LazyList.iterate(4)(_ + 1).map(init.withElfAp).map(game(stopIfElfDies = true))
    .dropWhile(_.deadElves > 0)
    .map(outcome)
    .head

  println(p2)
  assert(p2 == 45539)
}
