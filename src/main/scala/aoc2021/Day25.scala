package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day25 extends App {

  case class Herd(id: Char, locations: Set[(Int, Int)], dx: Int, dy: Int)

  case class Grid(herds: List[Herd], width: Int, height: Int)

  val input: List[String] = Using(Source.fromFile("inputs/2021/25.txt"))(_.getLines().toList).get

  def find(ch: Char): Set[(Int, Int)] = {
    input.zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.flatMap {
        case (c, x) if c == ch => Some(x, y)
        case _ => None
      }
    }.toSet
  }

  def herd(ch: Char, dx: Int, dy: Int) = Herd(ch, find(ch), dx, dy)

  def step(grid: Grid, herd: Herd): Herd = {
    val newLocations = for {
      (x, y) <- herd.locations
      newX = (x + herd.dx) % grid.width
      newY = (y + herd.dy) % grid.height
    } yield if (grid.herds.exists(_.locations(newX, newY))) (x, y) else (newX, newY)
    herd.copy(locations = newLocations)
  }

  def replace(grid: Grid, herd: Herd): Grid = {
    grid.copy(herds = grid.herds.map(h => if (h.id == herd.id) herd else h))
  }

  def step(grid: Grid): Grid = {
    grid.herds.foldLeft(grid) {
      case (g, h) => replace(g, step(g, h))
    }
  }

  @tailrec
  def countSteps(grid: Grid, acc: Int): Int = {
    val newGrid = step(grid)
    if (newGrid == grid) acc
    else countSteps(newGrid, acc + 1)
  }

  val initialGrid = Grid(
    herds = List(herd('>', 1, 0), herd('v', 0, 1)),
    width = input.head.length,
    height = input.length
  )

  println(countSteps(initialGrid, 1))
}
