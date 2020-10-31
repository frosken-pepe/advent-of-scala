package aoc2016

import scala.io.Source

object Day07 extends App {

  case class Ipv7(supernet: List[String], hypernet: List[String])

  val input = Source.fromFile("inputs/2016/07.txt").getLines()
    .toList
    .map(_.split("[\\[\\]]").toList)
    .map(_.zipWithIndex)
    .map { q =>
      val (as, bs) = q.partition(_._2 % 2 == 0)
      Ipv7(as.map(_._1), bs.map(_._1))
    }

  def hasABBA(part: String): Boolean =
    part.sliding(4).map(_.toList).foldLeft(false) {
      case (carry, a :: b :: c :: d :: Nil) => carry || a != b && b == c && d == a
    }

  def supportsTLS(ip: Ipv7): Boolean = {
    ip.supernet.count(hasABBA) > 0 && ip.hypernet.count(hasABBA) == 0
  }

  println(input.count(supportsTLS))

  def findABA(part: String): List[String] = {
    part.sliding(3).map(_.toList).foldLeft(List[String]()) {
      case (carry, a :: b :: c :: Nil) if a != b && a == c => s"$a$b$a" :: carry
      case (carry, _) => carry
    }
  }

  def supportsSSL(ip: Ipv7): Boolean = {
    val abas = ip.supernet.flatMap(findABA)
    val babs = abas.map(_.toList).map {
      case a::b::_ => s"$b$a$b"
    }
    babs.foldLeft(false) {
      case (carry, bab) => carry || ip.hypernet.count(hy => hy.contains(bab)) > 0
    }
  }

  println(input.count(supportsSSL))
}
