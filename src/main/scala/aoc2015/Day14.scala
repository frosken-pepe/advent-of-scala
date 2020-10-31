package aoc2015

import scala.io.Source

object Day14 extends App {

  case class Reindeer(name: String, speed: Int, duration: Int, rest: Int)

  object Reindeer {
    private val reindeer = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

    def apply(s: String): Reindeer = s match {
      case reindeer(name, speed, duration, rest) => new Reindeer(name, speed.toInt, duration.toInt, rest.toInt)
    }
  }

  val input = Source.fromFile("inputs/2015/14.txt").getLines()
    .map(Reindeer.apply)
    .toList

  def dist(time: Int)(reindeer: Reindeer): Int = {
    val cycleTime = reindeer.duration + reindeer.rest
    val numFullCycles = time / cycleTime
    val remainingBurst = math.min(reindeer.duration, time % cycleTime)
    (numFullCycles * reindeer.duration + remainingBurst) * reindeer.speed
  }

  println(input.map(dist(2503)).max)

  val leadersAtTimes = (1 to 2503).map { t =>
    val distances = input.map(r => (r, dist(t)(r)))
    val maxDistance = distances.maxBy(_._2)._2
    distances.filter(d => d._2 == maxDistance).map(_._1.name)
  }.toList

  val collectedPoints = leadersAtTimes.flatten.groupBy(identity).map {
    case (k, v) => k -> v.length
  }

  println(collectedPoints.values.max)
}
