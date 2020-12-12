package aoc2018

import scala.collection.mutable
import scala.io.Source

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
    val lines = Source.fromFile("inputs/2018/15.txt").getLines().toList
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
    val result = unitOrder.foldLeft(state)(turn).removeDead().nextRound()
    if (stopOnDeadElf && result.deadElves > 0) result.copy(ended = true)
    else result
  }

  def turn(state: State, unitId: String): State = {
    if (state.ended) return state
    val maybeMe = state.units.filter(_.id == unitId).find(_.alive())
    if (maybeMe.isEmpty) return state
    val me = maybeMe.get
    val targets = state.units.filter(_.alive()).filter(him => him.elf != me.elf)
    if (targets.isEmpty) return state.copy(ended = true)
    val isInRangeOfTarget = me.pos.neighs().intersect(targets.map(_.pos)).nonEmpty
    if (isInRangeOfTarget) attack(me, state)
    else {
      val move = findBestMove(me, state, targets)
      if (move.isEmpty) return state
      val newMe = me.copy(pos = move.get)
      attack(newMe, state.replaceUnit(me, newMe))
    }
  }

  def attack(me: Unit, state: State): State = {
    val targetLoc = state.units
      .filter(_.alive())
      .filter(him => him.elf != me.elf)
      .filter(him => me.pos.neighs().contains(him.pos))
      .map(u => (u.hp, u.pos))
      .toList
      .minOption
      .map(_._2)
    if (targetLoc.isEmpty) return state
    val him = state.units.filter(u => u.pos == targetLoc.get).head
    state.replaceUnit(him, him.copy(hp = him.hp - me.ap)).removeDead()
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
    if (end.isEmpty) return None
    val visited = mutable.Set[Vec2]()
    val q = mutable.Queue[(Int, Vec2)]()
    visited += begin
    q.enqueue((0, begin))
    while (q.nonEmpty) {
      val (dist, cur) = q.dequeue()
      if (end.contains(cur)) return Some(dist)
      for (n <- cur.neighs().filter(state.unoccupied)) {
        if (!visited.contains(n)) {
          q.enqueue((dist + 1, n))
          visited += n
        }
      }
    }
    None
  }

  def game(stopIfElfDies: Boolean)(state: State): State =
    LazyList.iterate(state)(round(stopIfElfDies)).dropWhile(s => !s.ended).head

  def outcome(state: State): Int = (state.roundNo - 1) * state.units.toList.map(_.hp).sum

  println(outcome(game(stopIfElfDies = false)(init)))

  println(
    LazyList.iterate(4)(_ + 1).map(init.withElfAp).map(game(stopIfElfDies = true))
      .dropWhile(_.deadElves > 0)
      .map(outcome)
      .head
  )
}
