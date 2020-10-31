package aoc2016

import scala.io.Source

object Day01 extends App {

  val input = Source.fromFile("inputs/2016/01.txt").getLines().next()
    .split(", ")
    .map(_.toList)
    .map { case ch :: rest => (ch, rest.mkString.toInt)}
    .toList

  abstract sealed class Direction {
    def left: Direction
    def right: Direction
    def dx: Int
    def dy: Int
    def walk(coord: (Int, Int), steps: Int): (Int, Int) = (coord._1 + dx * steps, coord._2 + dy * steps)
    def walkCoords(coord: (Int, Int), steps: Int): List[(Int, Int)] = (0 until steps).map(s => walk(coord, s)).toList
  }
  case object N extends Direction {
    override def left: Direction = W
    override def right: Direction = E
    override def dx: Int = 0
    override def dy: Int = -1
  }
  case object E extends Direction {
    override def left: Direction = N
    override def right: Direction = S
    override def dx: Int = 1
    override def dy: Int = 0
  }
  case object W extends Direction {
    override def left: Direction = S
    override def right: Direction = N
    override def dx: Int = -1
    override def dy: Int = 0
  }
  case object S extends Direction {
    override def left: Direction = E
    override def right: Direction = W
    override def dx: Int = 0
    override def dy: Int = 1
  }

  val headquarterPos = input.foldLeft((N: Direction, (0,0))) {
    case ((dir,coord), ('L', steps)) => (dir.left, dir.left.walk(coord, steps))
    case ((dir,coord), ('R', steps)) => (dir.right, dir.right.walk(coord, steps))
  }._2

  println(math.abs(headquarterPos._1) + math.abs(headquarterPos._2))

  val temp = input.scanLeft((W: Direction, (0,0), List[(Int,Int)]())) {
    case ((dir,coord,visited), ('L', steps)) => (dir.left, dir.left.walk(coord, steps), visited ++ dir.left.walkCoords(coord, steps))
    case ((dir,coord,visited), ('R', steps)) => (dir.right, dir.right.walk(coord, steps), visited ++ dir.right.walkCoords(coord, steps))
  }.dropWhile {
    case (_, _, visited) => visited.size == visited.distinct.size
  }.head._3

  val actualHeadquarterPos = temp.filter(coord => temp.count(c => c == coord) > 1).head

  println(math.abs(actualHeadquarterPos._1) + math.abs(actualHeadquarterPos._2))
}
