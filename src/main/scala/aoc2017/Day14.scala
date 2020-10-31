package aoc2017

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {

  import Day10.knotHash

  val input = Source.fromFile("inputs/2017/14.txt").getLines().next()

  def bits(hexDigit: Char): Seq[Boolean] = {
    val int = Integer.parseInt("" + hexDigit, 16)
    for {j <- 3 to 0 by -1} yield ((1 << j) & int) != 0
  }

  val hashes = for {i <- 0 until 128; hash = knotHash(input + "-" + i)} yield hash

  val bitArray = hashes.map(_.flatMap(bits))

  println(bitArray.flatten.count(bit => bit))

  val regionLabels = bitArray.map(
    arr => arr.map(b => if (b) -1 else 0).toVector
  ).toVector

  def fill(initial: Vector[Vector[Int]], row: Int, col: Int, label: Int): Vector[Vector[Int]] = {
    val unfilledNeighbors = for {
      (r, c) <- List((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
      if r >= 0 && c >= 0 && r < 128 && c < 128
      if initial(r)(c) == -1
    } yield (r, c)
    unfilledNeighbors.foldLeft(fillCenter(initial, row, col, label)) {
      case (acc, neigh) => fill(acc, neigh._1, neigh._2, label)
    }
  }

  private def fillCenter(initial: Vector[Vector[Int]], row: Int, col: Int, label: Int): Vector[Vector[Int]] = {
    initial.zipWithIndex.map {
      case (vec, `row`) => vec.zipWithIndex.map {
        case (_, `col`) => label
        case (itm, _) => itm
      }
      case (vec, _) => vec
    }
  }

  @tailrec def labelRegions(initial: Vector[Vector[Int]], counter: Int): Vector[Vector[Int]] = {
    val nextUnfilled = (for {
      row <- initial.indices
      col <- initial(row).indices if initial(row)(col) == -1
    } yield (row, col)).headOption
    if (nextUnfilled.isEmpty) initial
    else {
      val (row, col) = nextUnfilled.get
      labelRegions(fill(initial, row, col, counter), counter + 1)
    }
  }

  println(labelRegions(regionLabels, 1).flatten.max)
}
