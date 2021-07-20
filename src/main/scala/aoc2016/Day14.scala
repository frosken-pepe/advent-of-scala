package aoc2016

import aoc2015.Day04.md5

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day14 extends App {

  val input = Using(Source.fromFile("inputs/2016/14.txt"))(_.getLines().next()).get

  @tailrec def repeatedly(s: String)(n: Int)(fn: String => String): String = {
    if (n == 0) s
    else repeatedly(fn(s))(n - 1)(fn)
  }

  def isSameChar(s: String): Boolean = s.toCharArray.toSet.size == 1

  def findRepeating(str: String, n: Int): Option[Char] = str.sliding(n).find(isSameChar).map(_.head)

  def findIndices(stretch: Int): LazyList[Int] = {

    val cache = mutable.Map[Int, String]()

    def md5cached(i: Int): String = {
      if (cache.contains(i)) cache(i)
      else {
        val result = repeatedly(input + i)(1 + stretch)(s => md5(s).toLowerCase)
        cache(i) = result
        result
      }
    }

    LazyList.unfold(0) { i => Some((md5cached(i), i + 1)) }.zipWithIndex
      .filter {
        case (hash, index) =>
          findRepeating(hash, 3) match {
            case Some(c) =>
              val target = List.fill(5)(c).mkString
              ((index + 1) to (index + 1000)).map(md5cached).exists(_.contains(target))
            case _ => false
          }
      }.map(_._2)
  }

  println(findIndices(0).drop(63).head)
  println(findIndices(2016).drop(63).head)
}
