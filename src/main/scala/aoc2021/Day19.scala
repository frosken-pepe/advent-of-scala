package aoc2021

import aoc2018.Day17.fixedPoint

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day19 extends App {

  def rot(p: (Int, Int, Int), sgn: Int, axis: Char, theta: Int): (Int, Int, Int) = {
    val q = axis match {
      case 'x' => rotX(p, theta)
      case 'y' => rotY(p, theta)
      case 'z' => rotZ(p, theta)
    }
    face(q, sgn, axis)
  }

  def face(q: (Int, Int, Int), sgn: Int, axis: Char): (Int, Int, Int) = (sgn, axis) match {
    case (1, 'x') => q
    case (1, 'y') => rotZ(q, 270)
    case (1, 'z') => rotY(q, 90)
    case (-1, 'x') => rotZ(q, 180)
    case (-1, 'y') => rotZ(q, 90)
    case (-1, 'z') => rotY(q, 270)
  }

  @tailrec def cos(theta: Int): Int = theta match {
    case 0 => 1
    case 90 => 0
    case 180 => -1
    case 270 => 0
    case _ => cos(theta - 360)
  }

  @tailrec def sin(theta: Int): Int = theta match {
    case 0 => 0
    case 90 => 1
    case 180 => 0
    case 270 => -1
    case _ => sin(theta - 360)
  }

  def mul(M: ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int)), x: (Int, Int, Int)): (Int, Int, Int) = {
    (
      M._1._1 * x._1 + M._1._2 * x._2 + M._1._3 * x._3,
      M._2._1 * x._1 + M._2._2 * x._2 + M._2._3 * x._3,
      M._3._1 * x._1 + M._3._2 * x._2 + M._3._3 * x._3
    )
  }

  def rotX(p: (Int, Int, Int), theta: Int): (Int, Int, Int) = {
    val Rx = (
      (1, 0, 0),
      (0, cos(theta), -sin(theta)),
      (0, sin(theta), cos(theta))
    )
    mul(Rx, p)
  }

  def rotY(p: (Int, Int, Int), theta: Int): (Int, Int, Int) = {
    val Ry = (
      (cos(theta), 0, sin(theta)),
      (0, 1, 0),
      (-sin(theta), 0, cos(theta))
    )
    mul(Ry, p)
  }

  def rotZ(p: (Int, Int, Int), theta: Int): (Int, Int, Int) = {
    val Rz = (
      (cos(theta), -sin(theta), 0),
      (sin(theta), cos(theta), 0),
      (0, 0, 1)
    )
    mul(Rz, p)
  }

  def _orientations(p: (Int, Int, Int)): List[(Int, Int, Int)] = {
    for {
      sgn <- List(1, -1)
      axis <- List('x', 'y', 'z')
      theta <- List(0, 90, 180, 270)
    } yield rot(p, sgn, axis, theta)
  }

  case class Scanner(id: Int, orientationId: Int, beacons: List[(Int, Int, Int)]) {
    def orientations: List[Scanner] = {
      (0 until 24).map(_ + orientationId).map(_ % 24).map(i => copy(
        beacons = beacons.map(_orientations(_)(i)),
        orientationId = i
      )).toList
    }
  }

  object Scanner {
    def parse(source: String): Scanner = {
      val list = source.split("\n").toList
      val id = list.head match {
        case s"--- scanner $id ---" => id.toInt
      }
      Scanner(id, 0, list.tail.map {
        case s"$x,$y,$z" => (x.toInt, y.toInt, z.toInt)
      })
    }
  }

  val input = Using(Source.fromFile("inputs/2021/19.txt"))(_.getLines()
    .toList.mkString("\n").split("\n\n").map(Scanner.parse).toList
    .map(scanner => scanner.id -> scanner)
    .toMap).get


  def findOffset(s: Scanner, t: Scanner): Option[(Int, Int, Int)] = {
    val diffs = for {
      a <- s.beacons
      b <- t.beacons
      diff = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
    } yield diff
    diffs.groupBy(identity).map {
      case k -> v => k -> v.size
    }.filter(_._2 >= 12).keySet.headOption
  }

  def orient(r: (Int, Int, Int), orientationId: Int): (Int, Int, Int) = {
    _orientations(r)(orientationId)
  }

  def locateScanner(a: Scanner, b: Scanner): Option[(Int, (Int, Int, Int))] = {
    (for {
      o <- b.orientations
      r <- findOffset(a, o)
      xy = orient(r, a.orientationId)
    } yield (o.orientationId, xy)).headOption
  }

  def addOrientations(a: Int, b: Int): Int = {
    val target = orient(orient((1, 2, 3), b), a)
    _orientations((1, 2, 3)).zipWithIndex.find(_._1 == target).map(_._2).head
  }

  def inferScannerPositions(known: Map[Int, (Int, (Int, Int, Int))]): Map[Int, (Int, (Int, Int, Int))] = {
    val inferred = (for {
      a <- known.keys.map(input).map(s => s.copy(orientationId = known(s.id)._1))
      b <- input.keys.filter(k => !known.keySet(k)).map(input)
      (orientation, offset) <- locateScanner(a, b)
      pos = (known(a.id)._2._1 + offset._1, known(a.id)._2._2 + offset._2, known(a.id)._2._3 + offset._3)
    } yield (b.id, (addOrientations(a.orientationId, orientation), pos))).toMap
    known ++ inferred
  }

  val initialScannerPositions = Map(0 -> (0, (0, 0, 0)))

  val finalPos = fixedPoint(inferScannerPositions)(initialScannerPositions)

  println(
    (for {
      _ <- Set(())
      (id, (orientationId, position)) <- finalPos
      scanner = input(id)
      beacon <- scanner.beacons.map(orient(_, orientationId)).map(localBeacon =>
        (position._1 + localBeacon._1, position._2 + localBeacon._2, position._3 + localBeacon._3)
      )
    } yield beacon).size)

  println(
    (for {
      (_, (_, a)) <- finalPos
      (_, (_, b)) <- finalPos if a != b
    } yield (math.abs(a._1 - b._1) + math.abs(a._2 - b._2) + math.abs(a._3 - b._3))).max)
}
