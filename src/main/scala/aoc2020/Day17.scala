package aoc2020

import scala.io.Source

object Day17 extends App {

  case class Vec4(x: Int, y: Int, z: Int, w: Int) {
    def neighs(nd: Int): Set[Vec4] = (for {
      dx <- -1 to 1
      dy <- -1 to 1
      dz <- -1 to 1
      dw <- -1 to 1
      nx = x + dx
      ny = y + dy
      nz = if (nd >= 3) z + dz else 0
      nw = if (nd >= 4) w + dw else 0
    } yield Vec4(nx, ny, nz, nw)).toSet - Vec4(x, y, z, w)
  }

  def update(active: Boolean, activeNeighs: Int): Boolean = (active, activeNeighs) match {
    case (true, 2) => true
    case (true, 3) => true
    case (true, _) => false
    case (false, 3) => true
    case (false, _) => false
  }

  val input: Set[Vec4] = {
    Source.fromFile("inputs/2020/17.txt").getLines().toList.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.flatMap {
        case ('#', x) => Some(Vec4(x, y, 0, 0))
        case _ => None
      }
    }.toSet
  }

  def evolve(nd: Int)(state: Set[Vec4]) = for {
    n <- state
    c <- n.neighs(nd) + n
    active = state contains c
    activeNeighs = c.neighs(nd).count(state.contains)
    if update(active, activeNeighs)
  } yield c

  def conway(nd: Int): Int = {
    LazyList.iterate(input)(evolve(nd)).drop(6).map(_.size).head
  }

  println(conway(3))
  println(conway(4))
}
