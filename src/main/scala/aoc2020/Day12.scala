package aoc2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12 extends App {

  val input = Using(Source.fromFile("inputs/2020/12.txt"))(_.getLines().toList).get

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

  def forward(face: Char, x: Int, y: Int, amt: Int): (Char, Int, Int) = face match {
    case 'N' => (face, x, y + amt)
    case 'E' => (face, x + amt, y)
    case 'S' => (face, x, y - amt)
    case 'W' => (face, x - amt, y)
  }

  def ship(ship: (Char, Int, Int), s: String): (Char, Int, Int) = {
    val (face, x, y) = ship
    val (cmd, amt) = (s.head, s.tail.toInt)
    cmd match {
      case 'N' => (face, x, y + amt)
      case 'S' => (face, x, y - amt)
      case 'E' => (face, x + amt, y)
      case 'W' => (face, x - amt, y)
      case 'L' => (left(face, amt / 90), x, y)
      case 'R' => (right(face, amt / 90), x, y)
      case 'F' => forward(face, x, y, amt)
    }
  }

  val shipP1 = input.foldLeft(('E', 0, 0))(ship)

  println(shipP1._2.abs + shipP1._3.abs)

  @tailrec def rotateR(x: Int, y: Int, times: Int): (Int, Int) =
    if (times == 0) (x, y)
    else rotateR(y, -x, times - 1)

  @tailrec def rotateL(x: Int, y: Int, times: Int): (Int, Int) = {
    if (times == 0) (x, y)
    else rotateL(-y, x, times - 1)
  }

  def waypoint(param: (Int, Int, Int, Int), s: String): (Int, Int, Int, Int) = {
    val (shipX, shipY, wpX, wpY) = param
    val (cmd, amt) = (s.head, s.tail.toInt)
    cmd match {
      case 'N' => (shipX, shipY, wpX, wpY + amt)
      case 'S' => (shipX, shipY, wpX, wpY - amt)
      case 'E' => (shipX, shipY, wpX + amt, wpY)
      case 'W' => (shipX, shipY, wpX - amt, wpY)
      case 'L' =>
        val wp = rotateL(wpX, wpY, amt / 90)
        (shipX, shipY, wp._1, wp._2)
      case 'R' =>
        val wp = rotateR(wpX, wpY, amt / 90)
        (shipX, shipY, wp._1, wp._2)
      case 'F' => (shipX + wpX * amt, shipY + wpY * amt, wpX, wpY)
    }
  }

  val shipP2 = input.foldLeft((0, 0, 10, 1))(waypoint)

  println(shipP2._1.abs + shipP2._2.abs)
}
