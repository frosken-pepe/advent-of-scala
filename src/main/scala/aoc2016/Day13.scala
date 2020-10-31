package aoc2016

import scala.collection.mutable
import scala.io.Source

object Day13 extends App {

  val input = Source.fromFile("inputs/2016/13.txt").getLines().map(_.toInt).next()

  def isOpen(coord: (Int, Int)): Boolean = {
    val (x, y) = coord
    val find = (x * x + 3 * x + 2 * x * y + y + y * y + input).toLong.toBinaryString.count(_ == '1')
    find % 2 == 0
  }

  def neighs(coord: (Int, Int)): List[(Int, Int)] = {
    for {
      delta <- List(-1, 1)
      c <- List((coord._1 + delta, coord._2), (coord._1, coord._2 + delta))
      if isOpen(c) && c._1 >= 0 && c._2 >= 0
    } yield c
  }

  def bfs(start: (Int, Int), end: (Int, Int), max: Int): (Int, Int) = {
    val q = mutable.Queue[(Int, (Int, Int))]()
    val visited = mutable.Set[(Int, Int)]()
    q.enqueue((0, start))
    visited.add(start)
    var count = 0
    while (q.nonEmpty) {
      val (dist, coord) = q.dequeue()
      if (dist <= max) count += 1
      if (coord == end) return (dist, count)
      for {neigh <- neighs(coord)} {
        if (!visited.contains(neigh)) {
          visited.add(neigh)
          q.enqueue((dist + 1, neigh))
        }
      }
    }
    (-1, -1)
  }

  bfs((1, 1), (31, 39), 50) match {
    case (a, b) => println(a); println(b)
  }
}
