package aoc2015

import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter
import scala.io.Source
import scala.util.Using

object Day04 extends App {

  val input = Using(Source.fromFile("inputs/2015/04.txt"))(_.getLines().next()).get

  def md5(s: String): String = DatatypeConverter
    .printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes()))

  def mine(zeroes: Int): Int = LazyList.iterate(0)(_ + 1).filter {
    no => md5(input + no).startsWith("0" * zeroes)
  }.head

  println(mine(5))
  println(mine(6))
}
