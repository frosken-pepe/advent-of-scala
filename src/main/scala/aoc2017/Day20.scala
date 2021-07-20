package aoc2017

import scala.io.Source
import scala.util.Using

object Day20 extends App {

  type Vec3 = (Int, Int, Int)

  def add(a: Vec3, b: Vec3): Vec3 = (a._1 + b._1, a._2 + b._2, a._3 + b._3)

  case class Particle(id: Int, p: Vec3, v: Vec3, a: Vec3) {
    def move: Particle = {
      val newV = add(v, a)
      val newP = add(p, newV)
      copy(v = newV, p = newP)
    }
  }

  object Particle {

    private def manhattan(v: Vec3) = v._1.abs + v._2.abs + v._3.abs

    implicit val particleOrdering: Ordering[Particle] = (x: Particle, y: Particle) => {
      8 * Integer.compare(manhattan(x.a), manhattan(y.a)) +
        4 * Integer.compare(manhattan(x.v), manhattan(y.v)) +
        2 * Integer.compare(manhattan(x.p), manhattan(y.p)) +
        1 * Integer.compare(x.id, y.id)
    }
  }

  val re = """p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>""".r

  val input = Using(Source.fromFile("inputs/2017/20.txt"))(_.getLines()
    .zipWithIndex
    .map { case (re(px, py, pz, vx, vy, vz, ax, ay, az), id) => Particle(id, (px.toInt, py.toInt, pz.toInt), (vx.toInt, vy.toInt, vz.toInt), (ax.toInt, ay.toInt, az.toInt)) }
    .toIndexedSeq).get

  println(input.min.id)

  def update(particles: IndexedSeq[Particle]): Option[IndexedSeq[Particle]] = {
    val remaining = particles
      .map(_.move)
      .groupBy(_.p)
      .filter { case (_, parts) => parts.length == 1 }
      .flatMap(_._2)
      .toIndexedSeq
    if (particles.size != remaining.size || particles.sorted.map(_.id) != remaining.sorted.map(_.id)) Some(remaining)
    else None
  }

  println(LazyList.unfold(input)(update(_).map { p => (p.size, p) }).last)
}
