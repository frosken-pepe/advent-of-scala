package aoc2017

import scala.collection.immutable.Queue

object Assembly {

  case class CPU(id: String, memory: Map[Reg, Long], pc: Int, lastSound: Long, halt: Boolean, receive: Option[Long], send: Queue[Long], sent: Int, multiplications: Int) {
    def get(b: Ref): Long = b match {
      case r@Reg(_) => memory(r)
      case Const(value) => value
    }

    def next: CPU = copy(pc = pc + 1)

    def exec(program: IndexedSeq[Instruction]): CPU = {
      if (pc >= 0 && pc < program.length) program(pc).exec(this)
      else copy(halt = true)
    }

    def set(a: Reg, b: Ref): CPU = copy(memory = memory.updated(a, get(b)))

    def acc(a: Reg, b: Ref, op: (Long, Long) => Long): CPU = copy(memory = memory.updated(a, op(get(a), get(b))))
  }

  sealed abstract class Ref

  object Ref {
    def apply(s: String): Ref = s match {
      case s if s.head.isLetter => Reg(s)
      case s => Const(s.toInt)
    }
  }

  case class Reg(reg: String) extends Ref {
    if (reg.length != 1 || !reg.head.isLetter) throw new IllegalStateException(s"register must be a letter, was $reg")
  }

  case class Const(value: Int) extends Ref

  sealed abstract class Instruction {
    def exec(cpu: CPU): CPU
  }

  object Instruction {
    private val noop = """noop""".r
    private val set = """set (\w) (\w|-?\d+)""".r
    private val mul = """mul (\w) (\w|-?\d+)""".r
    private val jgz = """jgz (\w|-?\d+) (\w|-?\d+)""".r
    private val jnz = """jnz (\w|-?\d+) (\w|-?\d+)""".r
    private val add = """add (\w) (\w|-?\d+)""".r
    private val sub = """sub (\w) (\w|-?\d+)""".r
    private val mod = """mod (\w) (\w|-?\d+)""".r
    private val snd = """snd (\w)""".r
    private val rcv = """rcv (\w)""".r

    def apply(part: Int)(s: String): Instruction = s match {
      case noop() => NOOP
      case set(a, b) => SET(Reg(a), Ref(b))
      case mul(a, b) => MUL(Reg(a), Ref(b))
      case jgz(a, b) => JGZ(Ref(a), Ref(b))
      case jnz(a, b) => JNZ(Ref(a), Ref(b))
      case add(a, b) => ADD(Reg(a), Ref(b))
      case sub(a, b) => SUB(Reg(a), Ref(b))
      case mod(a, b) => MOD(Reg(a), Ref(b))
      case snd(a) if part == 1 => SNDP1(Reg(a))
      case rcv(a) if part == 1 => RCVP1(Reg(a))
      case snd(a) if part == 2 => SNDP2(Reg(a))
      case rcv(a) if part == 2 => RCVP2(Reg(a))
    }
  }

  case object NOOP extends Instruction {
    override def exec(cpu: CPU): CPU = cpu.next
  }

  case class SET(a: Reg, b: Ref) extends Instruction {
    override def exec(cpu: CPU): CPU = cpu.next.set(a, b)
  }

  case class MUL(a: Reg, b: Ref) extends Instruction {
    override def exec(cpu: CPU): CPU = cpu.next.acc(a, b, _ * _).copy(multiplications = cpu.multiplications + 1)
  }

  case class JGZ(a: Ref, b: Ref) extends Instruction {
    override def exec(cpu: CPU): CPU = if (cpu.get(a) > 0) cpu.copy(pc = cpu.pc + cpu.get(b).toInt) else cpu.next
  }

  case class JNZ(a: Ref, b: Ref) extends Instruction {
    override def exec(cpu: CPU): CPU = if (cpu.get(a) != 0) cpu.copy(pc = cpu.pc + cpu.get(b).toInt) else cpu.next
  }

  case class ADD(a: Reg, b: Ref) extends Instruction {
    override def exec(cpu: CPU): CPU = cpu.next.acc(a, b, _ + _)
  }

  case class SUB(a: Reg, b: Ref) extends Instruction {
    override def exec(cpu: CPU): CPU = cpu.next.acc(a, b, _ - _)
  }

  case class MOD(a: Reg, b: Ref) extends Instruction {
    override def exec(cpu: CPU): CPU = cpu.next.acc(a, b, _ % _)
  }

  case class SNDP1(a: Reg) extends Instruction {
    override def exec(cpu: CPU): CPU = cpu.next.copy(lastSound = cpu.get(a))
  }

  case class RCVP1(a: Reg) extends Instruction {
    override def exec(cpu: CPU): CPU = if (cpu.get(a) != 0) cpu.copy(halt = true) else cpu.next
  }

  case class SNDP2(a: Reg) extends Instruction {
    override def exec(cpu: CPU): CPU = cpu.next.copy(send = cpu.send.enqueue(cpu.get(a)), sent = cpu.sent + 1)
  }

  case class RCVP2(a: Reg) extends Instruction {
    override def exec(cpu: CPU): CPU = {
      if (cpu.receive.isDefined)
        cpu.next.copy(memory = cpu.memory.updated(a, cpu.receive.get), receive = None)
      else cpu.copy(halt = true)
    }
  }

  def runToHalt(program: Array[Instruction])(cpu: CPU): CPU = {
    LazyList.iterate(cpu) { cpu =>
      cpu.exec(program) }.dropWhile(cpu => !cpu.halt).head
  }
}
