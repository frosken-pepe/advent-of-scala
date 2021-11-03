package aoc2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val input = Using(Source.fromFile("inputs/2019/06.txt"))(_.getLines()
    .map(_.split("\\)").toList)
    .map(list => (list.tail.head, list.head))
    .toMap).get

  val objects = input.keys.toList

  @tailrec def depth(obj: String, acc: Int): Int = {
    if (obj == "COM") acc
    else depth(input(obj), acc + 1)
  }

  println(objects.map(depth(_, 0)).sum)

  @tailrec def path(cur: String, acc: List[String]): List[String] = {
    if (cur == "COM") acc.reverse
    else path(input(cur), cur :: acc)
  }

  val you = path("YOU", Nil)
  val san = path("SAN", Nil)

  def distanceToFirstCommonAncestor(p: List[String], q: List[String]): Int = {
    val qHash = q.toSet
    p.zipWithIndex.dropWhile(pair => !qHash.contains(pair._1)).head._2 - 1
  }

  println(distanceToFirstCommonAncestor(you, san) + distanceToFirstCommonAncestor(san, you))
}
