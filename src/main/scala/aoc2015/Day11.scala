package aoc2015

import scala.io.Source

object Day11 extends App {

  val input = Source.fromFile("inputs/2015/11.txt").getLines().next()

  def hasIncreasingSequence(pass: String): Boolean =
    pass.sliding(3).filter(p => p(2) == p(1) + 1 && p(1) == p(0) + 1).hasNext

  def allValidLetters(pass: String): Boolean =
    !pass.contains("i") && !pass.contains("o") && !pass.contains("l")

  def hasTwoPairs(pass: String): Boolean =
    pass.sliding(2).filter(s => s(0) == s(1)).toSet.size > 1

  def isValidPass(pass: String): Boolean =
    hasIncreasingSequence(pass) && allValidLetters(pass) && hasTwoPairs(pass)

  def incrMod(ints: Seq[Int], mod: Int): Seq[Int] = ints.foldRight((1, List[Int]())) {
    case (item, (carry, acc)) =>
      val sum = item + carry
      val nextCarry = if (sum >= mod) 1 else 0
      (nextCarry, sum % mod :: acc)
  }._2

  def nextPass(s: String): String =
    incrMod(s.map(ch => ch - 'a'), 26).map(ch => (ch + 'a').toChar).mkString

  def nextValidPass(currentPass: String): String =
    LazyList.iterate(currentPass)(nextPass).tail.filter(isValidPass).head

  val part1 = nextValidPass(input)

  println(part1)

  println(nextValidPass(part1))
}
