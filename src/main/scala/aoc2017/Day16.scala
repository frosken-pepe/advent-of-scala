package aoc2017

import scala.io.Source

object Day16 extends App {

  val spin = """s(\d+)""".r
  val exchange = """x(\d+)/(\d+)""".r
  val partner = """p(\w)/(\w)""".r

  val input = Source.fromFile("inputs/2017/16.txt").getLines().next().split(",")

  val initialArrangement = ('a' to 'p').mkString
  def justDance(initialArrangement: String): String = {
    input.foldLeft(initialArrangement) {
      case (arr, move) => move match {
        case spin(n) => val (a, b) = arr.splitAt(initialArrangement.length - n.toInt); b + a
        case exchange(a, b) => arr.zipWithIndex.map {
          case (_, idx) if idx == a.toInt => arr(b.toInt)
          case (_, idx) if idx == b.toInt => arr(a.toInt)
          case (c, _) => c
        }.mkString
        case partner(a, b) => arr.map { ch =>
          if (ch == a.head) b.head
          else if (ch == b.head) a.head
          else ch
        }
      }
    }
  }

  println(justDance(initialArrangement))

  val period = LazyList.iterate(initialArrangement)(justDance)
    .zipWithIndex
    .drop(1)
    .dropWhile(_._1 != initialArrangement)
    .head
    ._2

  println(LazyList.iterate(initialArrangement)(justDance)
    .drop(1_000_000_000 % period)
    .head)
}
