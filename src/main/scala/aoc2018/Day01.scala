package aoc2018

import scala.io.Source

object Day01 extends App {

  val input = Source.fromFile("inputs/2018/01.txt").getLines().map(_.toInt).toList
  println(input.sum)

  val seq = LazyList.iterate(0)(_ + 1).map(_ % input.length).map(input)

  println(
    seq.scanLeft((0, Set[Int](), List[Int]())) {
      case ((acc, seen, repeats), curr) =>
        val next = acc + curr
        if (seen.contains(next)) (next, seen, next :: repeats)
        else (next, seen + next, repeats)
    }.map(_._3).dropWhile(_.isEmpty).map(_.head).head
  )
}
