package aoc2016

import scala.io.Source

object Day21 extends App {

  val input = Source.fromFile("inputs/2016/21.txt").getLines()
    .map(operation)
    .toList

  def rotateLeft[A](seq: Seq[A], i: Int): Seq[A] = {
    val size = seq.size
    seq.drop(i % size) ++ seq.take(i % size)
  }

  def rotateRight[A](seq: Seq[A], i: Int): Seq[A] = {
    val size = seq.size
    seq.drop(size - (i % size)) ++ seq.take(size - (i % size))
  }

  def operation(op: String): String => String = { p =>

    val rr = """rotate right (\d+) steps?""".r
    val rl = """rotate left (\d+) steps?""".r
    val swapPos = """swap position (\d+) with position (\d+)""".r
    val swapLetter = """swap letter (\w) with letter (\w)""".r
    val rotateBased = """rotate based on position of letter (\w)""".r
    val reverse = """reverse positions (\d+) through (\d+)""".r
    val move = """move position (\d+) to position (\d+)""".r

    val result = op match {
      case rr(steps) => rotateRight(p, steps.toInt).mkString
      case rl(steps) => rotateLeft(p, steps.toInt).mkString
      case swapPos(a, b) => p.zipWithIndex.map {
        case (_, idx) if idx == a.toInt => p(b.toInt)
        case (_, idx) if idx == b.toInt => p(a.toInt)
        case (ch, _) => ch
      }.mkString
      case rotateBased(c) =>
        val pos = p.indexOf(c)
        rotateRight(p, 1 + pos + (if (pos >= 4) 1 else 0)).mkString
      case swapLetter(a, b) => p map {
        case ch if ch == a.head => b.head
        case ch if ch == b.head => a.head
        case ch => ch
      }
      case reverse(s, e) =>
        val (start, end) = (s.toInt, e.toInt)
        p.take(start) + p.substring(start, end + 1).reverse + p.drop(end + 1)
      case move(f, t) =>
        val (from, to) = (f.toInt, t.toInt)
        val range = (math.min(from, to), math.max(from, to))
        val z = p.substring(range._1, range._2 + 1)
        p.take(range._1) + (if (from < to) rotateLeft(z, 1) else rotateRight(z, 1)) + p.drop(range._2 + 1)
    }

    result
  }

  def scramble(password: String) = input.foldLeft(password) {
    case (acc, op) => op(acc)
  }

  println(scramble("abcdefgh"))

  println("abcdefgh".permutations.filter(perm => scramble(perm) == "fbgdceah").next())
}
