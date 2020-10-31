package aoc2015

import scala.io.Source

object Day19 extends App {

  val input = Source.fromFile("inputs/2015/19.txt").getLines().toList

  val replacement = """(\w+) => (\w+)""".r

  val replacements = input.flatMap {
    _ match {
      case replacement(src, tar) => Some((src, tar))
      case _ => None
    }
  }

  val molecule = input.last

  def replaceAllIn(s: String, from: String, to: String): Set[String] = {
    val parts = s"<$s>".split(from).toList

    def glue(pos: Int): String = {
      (parts.take(pos).mkString(from) + to + parts.drop(pos).mkString(from)).drop(1).dropRight(1)
    }

    (1 until parts.length).map(glue).toSet
  }

  println(replacements.flatMap {
    case (from, to) => replaceAllIn(molecule, from, to)
  }.distinct.size)

  println(
    LazyList.iterate((0, molecule)) { case (i, s) =>
      val candidates = (for {
        (from, to) <- replacements
        candidate <- replaceAllIn(s, to, from)
      } yield candidate).toSet
      (i + 1, candidates.minBy(_.length))
    }.dropWhile(_._2 != "e").head._1)
}
