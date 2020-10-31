package aoc2016

import scala.io.Source

object Day10 extends App {

  case class Bot(id: Int, lo: Ref, hi: Ref) extends Ref

  sealed class Ref

  object Ref {
    def apply(s: String, id: String): Ref = s match {
      case "bot" => BotRef(id.toInt)
      case "output" => OutputRef(id.toInt)
    }
  }

  case class BotRef(id: Int) extends Ref

  case class OutputRef(id: Int) extends Ref

  val input = Source.fromFile("inputs/2016/10.txt").getLines().toList

  val valueAssignment = """value (\d+) goes to bot (\d+)""".r
  val initialValueAssignments = input.filter(_.startsWith("value"))
    .foldLeft(Map[Ref, List[Int]]() withDefaultValue Nil) {
      case (acc, valueAssignment(value, bot)) =>
        val (b, v) = (BotRef(bot.toInt), value.toInt)
        acc.updated(b, (v :: acc(b)).sorted)
    }

  val bot = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r
  val botsById: Map[Ref, Bot] = input.filter(_.startsWith("bot")).map {
    case bot(id, typeLo, idLo, typeHi, idHi) => Bot(id.toInt, Ref(typeLo, idLo), Ref(typeHi, idHi))
  }.map { bot => (BotRef(bot.id), bot) }.toMap

  private def next(ass: Map[Ref, List[Int]]): Map[Ref, List[Int]] = {
    (for {
      (ref, values) <- ass.filter(_._2.size == 2)
      lo :: hi :: Nil = values
      (value, targetRef) <- List((lo, botsById(ref).lo), (hi, botsById(ref).hi))
    } yield (ref, value, targetRef)).foldLeft(ass) {
      case (acc, (src, value, target)) => acc.updated(src, Nil).updated(target, (value :: acc(target)).sorted)
    }
  }

  val ll = LazyList.iterate(initialValueAssignments)(next)

  val part1 = ll.filter(map => map.values.foldLeft(false) {
    case (acc, list) => acc || list == List(17, 61)
  }).head.filter {
    case (_, v) => v == List(17, 61)
  }.head._1

  println(part1 match { case BotRef(id) => id })

  val part2 = ll.filter(map =>
    map.contains(OutputRef(0)) && map.contains(OutputRef(1)) && map.contains(OutputRef(2))
  ).head

  println(part2(OutputRef(0)).head * part2(OutputRef(1)).head * part2(OutputRef(2)).head)
}
