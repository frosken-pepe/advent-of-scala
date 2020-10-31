package aoc2017

import scala.io.Source

object Day09 extends App {

  val input = Source.fromFile("inputs/2017/09.txt").getLines().next()

  type Parser[A] = String => Option[(A, String)]

  sealed trait Block

  case class Garbage(len: Int) extends Block

  case class Group(children: List[Block]) extends Block

  def char(ch: Char): Parser[Char] = { s =>
    if (s.nonEmpty && s.head == ch) Some((ch, s.tail))
    else None
  }

  def garbageContent(s: String, isEscape: Boolean = false): Option[(Int, String)] = {
    if (!isEscape && char('>')(s).isDefined) Some(0, s)
    else if (char('!')(s).isDefined && !isEscape) garbageContent(s.tail, isEscape = true)
    else if (isEscape) garbageContent(s.tail)
    else garbageContent(s.tail).map(pair => (1 + pair._1, pair._2))
  }

  def garbage(s: String): Option[(Garbage, String)] = for {
    (_, a) <- char('<')(s)
    (len, b) <- garbageContent(a)
    (_, rest) <- char('>')(b)
  } yield (Garbage(len), rest)

  def either[A](a: Parser[A], b: Parser[A]): Parser[A] = { s =>
    a(s).orElse(b(s))
  }

  def separatedBy[A](sep: Char)(a: Parser[A]): Parser[List[A]] = { s =>
    val entity = a(s)
    if (entity.isEmpty) Some((Nil, s))
    else {
      val hd = entity.get
      char(',')(hd._2).flatMap {
        case (_, rest) => separatedBy(sep)(a)(rest).map {
          case (acc, str) => (hd._1 :: acc, str)
        }
      }.orElse(Some(List(hd._1), hd._2))
    }
  }

  def list(s: String): Option[(List[Block], String)] = {
    separatedBy(',')(either(group, garbage))(s)
  }

  def group(s: String): Option[(Group, String)] = for {
    (_, a) <- char('{')(s)
    (list, b) <- list(a)
    (_, rest) <- char('}')(b)
  } yield (Group(list), rest)

  def score(group: Group): Int = {
    def scorePrivate(group: Group, level: Int): Int = {
      level + group.children.map {
        case Garbage(_) => 0
        case g@Group(_) => scorePrivate(g, level + 1)
      }.sum
    }

    scorePrivate(group, 1)
  }

  def countGarbage(entity: Block): Int = entity match {
    case Garbage(len) => len
    case Group(children) => children.map(countGarbage).sum
  }

  val parsed = group(input).filter(_._2.isEmpty).map(_._1)

  println(parsed.map(score))
  println(parsed.map(countGarbage))
}
