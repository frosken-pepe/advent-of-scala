package aoc2018

import aoc2018.Opcodes.{Registry, parseInstruction}

import scala.io.Source
import scala.util.Using

object Day19 extends App {

  val ip = """#ip (\d)""".r

  val lines = Using(Source.fromFile("inputs/2018/19.txt"))(_.getLines().toVector).get

  val ipRef = lines.head match {
    case ip(i) => i.toInt
  }

  val program = lines.tail.map(parseInstruction)

  case class CPU(ip: Int, registry: Registry) {
    private def advance: Option[CPU] = {
      println(registry)
      if (ip < 0 || ip > program.length) None
      else Some(CPU.resume(program(ip).exec(registry)))
    }

    def run: Registry = LazyList.unfold(this)(_.advance.map(cpu => (cpu.registry, cpu))).last
  }

  object CPU {
    private def resume(registry: Registry): CPU = {
      val ip = registry.get(ipRef) + 1
      CPU(ip, registry.set(ipRef, ip))
    }
  }

  //println(CPU(0, Registry(Vector.fill(6)(0))).run.get(0))
  println(CPU(0, Registry(Vector(1,0,0,0,0,0))).run.get(0))
}
