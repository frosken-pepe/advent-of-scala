package aoc2016

import scala.io.Source

object Day08 extends App {

  val w = 50
  val h = 6

  abstract sealed class ScreenInstr

  object ScreenInstr {
    private val rect = """rect (\d+)x(\d+)""".r
    private val rotateRow = """rotate row y=(\d+) by (\d+)""".r
    private val rotateCol = """rotate column x=(\d+) by (\d+)""".r

    def apply(s: String): ScreenInstr = s match {
      case rect(w, h) => Rect(w.toInt, h.toInt)
      case rotateRow(row, by) => RotateRow(row.toInt, by.toInt)
      case rotateCol(col, by) => RotateCol(col.toInt, by.toInt)
    }
  }

  case class Rect(w: Int, h: Int) extends ScreenInstr

  case class RotateRow(row: Int, by: Int) extends ScreenInstr

  case class RotateCol(col: Int, by: Int) extends ScreenInstr

  val input = Source.fromFile("inputs/2016/08.txt").getLines()
    .map(ScreenInstr.apply)
    .toList

  val init = Set[(Int, Int)]()

  val on = input.foldLeft(init) {
    case (on, Rect(w, h)) =>
      on ++ (0 until w).flatMap(x => (0 until h).map(y => (x, y))).toSet
    case (on, RotateRow(row, by)) =>
      val currentRow = on.filter(_._2 == row)
      val newRow = currentRow.map(coord => ((coord._1 + by) % w, row))
      (on -- currentRow) ++ newRow
    case (on, RotateCol(col, by)) =>
      val currentCol = on.filter(_._1 == col)
      val newCol = currentCol.map(coord => (col, (coord._2 + by) % h))
      (on -- currentCol) ++ newCol
  }

  println(on.size)
  println((0 until h).map(y => (0 until w).map {
    case x if on.contains((x,y)) => '#'
    case _ => ' '
  }.mkString).mkString("\n"))
}
