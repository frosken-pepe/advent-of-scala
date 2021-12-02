package aoc2021

import scala.io.Source
import scala.util.Using

object Day02 extends App {

  val input = Using(Source.fromFile("inputs/2021/02.txt"))(_.getLines().toList).get

  println(input)

  val cmds = for {
    s <- input
    cmd :: amt :: Nil = s.split(" ").toList
  } yield (cmd, amt.toInt)

  val pos = cmds.foldLeft((0, 0)) {
    case ((h, d), ("forward", amt)) => (h + amt, d)
    case ((h, d), ("down", amt)) => (h, d + amt)
    case ((h, d), ("up", amt)) => (h, d - amt)
  }

  println(pos._1 * pos._2)

  val aim = cmds.foldLeft((0, 0, 0)) {
    case ((h, d, aim), ("forward", amt)) => (h + amt, d + aim * amt, aim)
    case ((h, d, aim), ("down", amt)) => (h, d, aim + amt)
    case ((h, d, aim), ("up", amt)) => (h, d, aim - amt)
  }

  println(aim._1 * aim._2)
}
