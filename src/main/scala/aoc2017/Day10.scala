package aoc2017

import scala.io.Source

object Day10 extends App {

  case class State(cur: Int, skip: Int, list: List[Int])

  private def hash(lengths: Seq[Int]): State = lengths.foldLeft(State(0, 0, (0 to 255).toList)) {
    case (State(cur, skip, list), length) =>
      val indices = (cur until cur + length).map(_ % list.length)
      val reversedSubList = indices.map(i => list(i)).reverse
      val newList = list.zipWithIndex.map {
        case (_, idx) if indices.contains(idx) => reversedSubList((list.length + idx - cur) % list.length)
        case (v, _) => v
      }
      State((cur + length + skip) % list.length, skip + 1, newList)
  }

  val input = Source.fromFile("inputs/2017/10.txt").getLines().next()
  val lengths = input.split(",").map(_.toInt).toList
  val h = hash(lengths).list
  println(h.head * h.tail.head)

  // reused in day 14
  def knotHash(input: String): String = {
    val lengths = input.map(_.toInt).toList ++ List(17, 31, 73, 47, 23)
    val sparseHash = hash(for {_ <- 1 to 64; j <- lengths} yield j).list
    val denseHash = sparseHash.sliding(16, 16).map(_.reduce(_ ^ _))
    denseHash.map(_.toHexString).map(s => if (s.length == 1) "0" + s else s).mkString
  }

  println(knotHash(input))
}
