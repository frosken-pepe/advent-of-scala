package aoc2018

import scala.io.Source
import scala.util.Using

object Day04 extends App {

  object Input {

    private val input = Using(Source.fromFile("inputs/2018/04.txt"))(_.getLines().toList.sorted).get

    private val timestamp = """\[1518-\d{2}-\d{2} (\d{2}):(\d{2})\] """
    private val beginShift = (timestamp + """Guard #(\d+) begins shift""").r
    private val fallsAsleep = (timestamp + "falls asleep").r
    private val wakesUp = (timestamp + "wakes up").r

    case class Zzz(guardId: Int, sleep: Int, wake: Int) {
      val asleep: Set[Int] = (for {i <- sleep until wake} yield i).toSet
    }

    val sleepers: List[Zzz] = input.foldLeft(List[Zzz]()) {
      case (acc, beginShift(_, _, guardId)) => Zzz(guardId.toInt, -1, -1) :: acc
      case (acc, fallsAsleep(hour, minute)) if hour == "00" => {
        if (acc.head.wake == -1) acc.head.copy(sleep = minute.toInt) :: acc.tail
        else acc.head.copy(sleep = minute.toInt) :: acc
      }
      case (acc, wakesUp(hour, minute)) if hour == "00" => acc.head.copy(wake = minute.toInt) :: acc.tail
    }
  }

  import Input._

  val mostSleepyGuard = sleepers
    .groupBy(_.guardId)
    .map { case (k, v) => (k, v.map(_.asleep.size).sum) }
    .maxBy(_._2)
    ._1

  def mostSleepyMinute(guardId: Int) = sleepers.filter(_.guardId == guardId).foldLeft(Map[Int, Int]()) {
    case (acc, zzz) => zzz.asleep.foldLeft(acc) {
      case (acc, min) => acc.updated(min, acc.getOrElse(min, 0) + 1)
    }
  }.maxByOption(_._2)

  println(mostSleepyGuard * mostSleepyMinute(mostSleepyGuard).get._1)

  println(sleepers
    .map(_.guardId)
    .flatMap(guardId => mostSleepyMinute(guardId).map(pair => (guardId * pair._1, pair)))
    .maxBy(_._2._2)
    ._1)
}
