package aoc2017

import scala.io.Source

object Day21 extends App {

  case class Pattern[T](private val data: IndexedSeq[IndexedSeq[T]]) {

    val sz: Int = data.length

    if (!data.forall(_.length == sz)) throw new IllegalArgumentException

    def get(row: Int, col: Int): T = data(row)(col)

    def rotate: Pattern[T] = copy(data = for {
      i <- 0 until sz
      row = (0 until sz).map(j => get(sz - j - 1, i))
    } yield row)

    def flipH: Pattern[T] = copy(data = data.map(_.reverse))

    def flipV: Pattern[T] = copy(data = data.reverse)

    def map[U](f: T => U): Pattern[U] = Pattern(data.map(_.map(f)))

    def flatMap[U](f: T => Pattern[U]): Pattern[U] = Pattern.flatten(map(f))

    def count(p: T => Boolean): Int = data.map(_.count(p)).sum

    def windowed(w: Int): Pattern[Pattern[T]] = {
      if (sz % w != 0) throw new IllegalArgumentException
      Pattern(for {
        rowNo <- 0 until sz by w
        window = (0 until sz by w).map(colNo => Pattern(for {
          i <- rowNo until rowNo + w
          row = (colNo until colNo + w).map(j => data(i)(j))
        } yield row))
      } yield window)
    }
  }

  object Pattern {
    private def flatten[T](nested: Pattern[Pattern[T]]): Pattern[T] = {
      val blockSz = if (nested.data.isEmpty) 0 else nested.get(0, 0).sz
      val targetSz = nested.sz * blockSz
      Pattern(for {
        i <- 0 until targetSz
        row = (0 until targetSz).map(j => nested.get(i / blockSz, j / blockSz).get(i % blockSz, j % blockSz))
      } yield row)
    }
  }

  case class EnhancementRule(input: Pattern[Boolean], output: Pattern[Boolean]) {
    val matches: Set[Pattern[Boolean]] = {
      Set(input, input.flipH, input.flipV).flatMap(p => LazyList.iterate(p)(_.rotate).take(4))
    }
  }

  object EnhancementRule {
    private val re = """(.*) => (.*)""".r

    private def pattern(s: String): Pattern[Boolean] = Pattern(s.split("/").map(_.map(ch => ch == '#')))

    def apply(s: String): EnhancementRule = s match {
      case re(src, dest) => new EnhancementRule(pattern(src), pattern(dest))
    }
  }

  val lookup = Source.fromFile("inputs/2017/21.txt").getLines()
    .map(EnhancementRule.apply)
    .flatMap(i => i.matches.map(m => (m, i.output))).toMap

  val init = Pattern(IndexedSeq(
    IndexedSeq(false, true, false),
    IndexedSeq(false, false, true),
    IndexedSeq(true, true, true),
  ))

  LazyList.iterate(init) { prev =>
    prev.windowed(if (prev.sz % 2 == 0) 2 else 3).flatMap(lookup)
  }.map(_.count(identity)).zipWithIndex.filter {
    case (_, i) => i == 5 || i == 18
  }.map(_._1).take(2).foreach(println)
}
