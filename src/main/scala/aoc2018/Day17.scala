package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day17 extends App {

  case class Vec2(x: Int, y: Int) {

    def down: Vec2 = Vec2(x, y + 1)

    def up: Vec2 = Vec2(x, y - 1)

    def left: Vec2 = Vec2(x - 1, y)

    def right: Vec2 = Vec2(x + 1, y)
  }

  val source = Vec2(500, 0)

  val clay = {
    val vertical = """x=(-?\d+), y=(-?\d+)\.\.(-?\d+)""".r
    val horizontal = """y=(-?\d+), x=(-?\d+)\.\.(-?\d+)""".r
    Source.fromFile("inputs/2018/17.txt").getLines().flatMap {
      case vertical(x, yMin, yMax) => (yMin.toInt to yMax.toInt).map(y => Vec2(x.toInt, y))
      case horizontal(y, xMin, xMax) => (xMin.toInt to xMax.toInt).map(x => Vec2(x, y.toInt))
    }.toSet
  }

  val minY = clay.map(_.y).min
  val maxY = clay.map(_.y).max
  val minX = clay.map(_.x).min
  val maxX = clay.map(_.x).max

  @tailrec def fixedPoint[T](update: T => T)(init: T): T = {
    val result = update(init)
    if (init == result) init
    else fixedPoint(update)(result)
  }

  val stable = {
    def update(move: Vec2 => Vec2)(cur: Set[Vec2]): Set[Vec2] = cur ++ (for {
      c <- cur
      next = move(c) if (cur contains next.down) && !(cur contains next)
    } yield next)

    (fixedPoint(update(_.left))(clay) intersect fixedPoint(update(_.right))(clay)) -- clay
  }

  case class State(settled: Set[Vec2], flowing: Set[Vec2]) {

    def count: Int = {
      (settled ++ flowing).count {
        case Vec2(_, y) => y >= minY && y <= maxY
      }
    }

    def viz(): Unit = {
      for {
        y <- (minY - 1) to (maxY + 1)
      } println(
        ((minX - 1) to (maxX + 1)).map { x =>
          val vec = Vec2(x, y)
          if (clay contains vec) '#'
          else if (flowing contains vec) '|'
          else if (settled contains vec) '~'
          else if (stable contains vec) '_'
          else '.'
        }.mkString)
      println()
    }

    def flow: State = {
      fixedPoint[State](s => s.flowAndSettleFP.flowSidewaysFP.soak)(this)
    }

    private def soak: State = {
      val newSettled = for {
        f <- settled
        n <- Set(f.down, f.right, f.left)
        if stable contains n
      } yield n
      State(settled ++ newSettled, flowing -- newSettled)
    }

    private def flowSidewaysFP: State = {
      fixedPoint[State](_.flowSideways)(this)
    }

    private def flowSideways: State = {
      // viz()
      val newFlowing = for {
        f <- flowing
        d = f.down if (settled contains d) || (clay contains d)
        n <- Set(f.left, f.right)
        if !(clay contains n)
        if !(flowing contains n)
        if !(settled contains n)
      } yield n
      State(settled, flowing ++ newFlowing)
    }

    private def flowAndSettleFP: State = {
      fixedPoint[State](_.flowAndSettle)(this)
    }

    private def flowAndSettle: State = {
      // viz()
      State(settled, flowing ++ (for {
        f <- flowing
        d = f.down
        if d.y <= maxY
        if !flowing.contains(d)
        if !clay.contains(d)
        if !settled.contains(d)
      } yield d)).settle
    }

    private def settle: State = {
      // viz()
      val newSettled = for {
        f <- flowing
        d = f.down
        if settled.contains(d) || clay.contains(d)
        if stable.contains(f)
        w <- fixedPoint(expandLR)(Set(f))
      } yield w
      State(settled ++ newSettled, flowing -- newSettled)
    }

    private def expandLR(f: Set[Vec2]): Set[Vec2] = f ++ (for {
      z <- f
      r <- Set(z.left, z.right)
      if stable contains r
    } yield r)
  }

  val initialState = State(Set(), Set(source))

  val flow = initialState.flow
  val p1 = flow.count
  println(p1)
  assert(p1 == 27736)

  val p2 = flow.settled.size
  println(p2)
  assert(p2 == 22474)
}
