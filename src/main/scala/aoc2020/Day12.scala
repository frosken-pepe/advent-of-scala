package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App {

  val input = Source.fromFile("inputs/2020/12.txt").getLines().toList

  @tailrec def left(dir: Char, times: Int): Char = {
    if (times == 0) dir
    else left(dir match {
      case 'N' => 'W'
      case 'E' => 'N'
      case 'S' => 'E'
      case 'W' => 'S'
    }, times - 1)
  }

  @tailrec def right(dir: Char, times: Int): Char = {
    if (times == 0) dir
    else right(dir match {
      case 'N' => 'E'
      case 'E' => 'S'
      case 'S' => 'W'
      case 'W' => 'N'
    }, times - 1)
  }

  def forward(x: Int, y: Int, face: Char, amt: Int): (Int, Int) = face match {
    case 'N' => (x, y - amt)
    case 'E' => (x + amt, y)
    case 'S' => (x, y + amt)
    case 'W' => (x - amt, y)
  }

  def ship(ship: (Char, Int, Int))(s: String): (Char, Int, Int) = {
    val (face, x, y) = ship
    if (s.head == 'N') (face, x, y - s.tail.toInt)
    else if (s.head == 'S') (face, x, y + s.tail.toInt)
    else if (s.head == 'E') (face, x + s.tail.toInt, y)
    else if (s.head == 'W') (face, x - s.tail.toInt, y)
    else if (s.head == 'L') (left(face, s.tail.toInt / 90), x, y)
    else if (s.head == 'R') (right(face, s.tail.toInt / 90), x, y)
    else if (s.head == 'F') {
      val f = forward(x, y, face, s.tail.toInt)
      (face, f._1, f._2)
    }
    else throw new IllegalArgumentException(s"??? $s")
  }

  val shipP1 = input.foldLeft(('E', 0, 0)) { case (acc, s) => ship(acc)(s) }

  println(shipP1._2.abs + shipP1._3.abs)

  @tailrec def rotateR(x: Int, y: Int, times: Int): (Int, Int) =
    if (times == 0) (x, y)
    else rotateR(y, -x, times - 1)

  @tailrec def rotateL(x: Int, y: Int, times: Int): (Int, Int) = {
    if (times == 0) (x, y)
    else rotateL(-y, x, times - 1)
  }

  def waypoint(param: (Int,Int,Int,Int))(s: String): (Int, Int, Int, Int) = {
    val (shipX, shipY, wpX, wpY) = param
    if (s.head == 'N') (shipX, shipY, wpX, wpY - s.tail.toInt)
    else if (s.head == 'S') (shipX, shipY, wpX, wpY + s.tail.toInt)
    else if (s.head == 'E') (shipX, shipY, wpX + s.tail.toInt, wpY)
    else if (s.head == 'W') (shipX, shipY, wpX - s.tail.toInt, wpY)
    else if (s.head == 'L') {
      val wp = rotateR(wpX, wpY, s.tail.toInt / 90) // dafuq
      (shipX, shipY, wp._1, wp._2)
    } else if (s.head == 'R' && s.tail.toInt % 90 == 0) {
      val wp = rotateL(wpX, wpY, s.tail.toInt / 90) // dafuq
      (shipX, shipY, wp._1, wp._2)
    } else if (s.head == 'F') {
      val amt = s.tail.toInt
      (shipX + wpX * amt, shipY + wpY * amt, wpX, wpY)
    }
    else throw new IllegalArgumentException(s"??? $s")
  }

  val shipP2 = input.foldLeft((0, 0, 10, -1)) { case (acc, s) => waypoint(acc)(s)}

  println(shipP2._1.abs + shipP2._2.abs)
}
