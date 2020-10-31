package aoc2016

object AssembunnyInterpreter {

  import scala.util.matching.Regex

  sealed abstract class Ref

  object Ref {
    private val number = """(-?\d+)""".r

    def apply(s: String): Ref = s match {
      case number(no) => Const(no.toLong)
      case _ => Reg(s)
    }
  }

  case class Reg(reg: String) extends Ref

  case class Const(value: Long) extends Ref

  case class CPU(pc: Long, memory: Map[Reg, Long], code: List[Assembunny], output: List[Long]) {
    def ref(ref: Ref): Long = ref match {
      case r@Reg(_) => memory(r)
      case Const(v) => v
    }
  }

  sealed abstract class Assembunny {
    def execute(cpu: CPU): CPU
  }

  object Assembunny {
    val cpy: Regex = """cpy (-?\w+) (-?\w+).*?""".r
    val inc: Regex = """inc ([a-d]).*?""".r
    val dec: Regex = """dec ([a-d]).*?""".r
    val jnz: Regex = """jnz (-?\w+) (-?\w+).*?""".r
    val tgl: Regex = """tgl (-?\w+).*?""".r
    val add: Regex = """add (-?\w+) (-?\w+).*?""".r
    val mul: Regex = """mul (-?\w+) (-?\w+).*?""".r
    val sub: Regex = """sub (-?\w+) (-?\w+).*?""".r
    val noop: Regex = """noop.*?""".r
    val out: Regex = """out (\w+)""".r
  }

  case class CPY(x: Ref, y: Ref) extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu.copy(
      pc = cpu.pc + 1,
      memory = y match {
        case r@Reg(_) => cpu.memory.updated(r, cpu.ref(x))
        case _ => cpu.memory
      }
    )
  }

  case class INC(x: Ref) extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu.copy(
      pc = cpu.pc + 1,
      memory = x match {
        case r@Reg(_) => cpu.memory.updated(r, cpu.ref(r) + 1)
        case _ => cpu.memory
      }
    )
  }

  case class DEC(x: Ref) extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu.copy(
      pc = cpu.pc + 1,
      memory = x match {
        case r@Reg(_) => cpu.memory.updated(r, cpu.ref(r) - 1)
        case _ => cpu.memory
      }
    )
  }

  case class JNZ(x: Ref, y: Ref) extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu match {
      case CPU(a, b, c, o) =>
        if (cpu.ref(x) != 0) CPU(a + cpu.ref(y), b, c, o)
        else CPU(a + 1, b, c, o)
    }
  }

  case class TGL(x: Ref) extends Assembunny {
    override def execute(cpu: CPU): CPU = {
      val i = cpu.pc + cpu.ref(x)
      val newInstruction: Option[Assembunny] = {
        if (i < 0 || i >= cpu.code.length) None
        else Some(
          cpu.code(i.toInt) match {
            case CPY(x, y) => JNZ(x, y)
            case INC(x) => DEC(x)
            case DEC(x) => INC(x)
            case JNZ(x, y) => CPY(x, y)
            case TGL(x) => INC(x)
          })
      }
      cpu.copy(pc = cpu.pc + 1, code = newInstruction match {
        case Some(instruction) => cpu.code.zipWithIndex.map {
          case (_, `i`) => instruction
          case (z, _) => z
        }
        case None => cpu.code
      })
    }
  }

  // optimizations

  // add y to x
  case class ADD(x: Reg, y: Ref) extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu.copy(
      pc = cpu.pc + 1,
      memory = cpu.memory.updated(x, cpu.ref(x) + cpu.ref(y)),
    )
  }

  // subtract y from x
  case class SUB(x: Reg, y: Ref) extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu.copy(
      pc = cpu.pc + 1,
      memory = cpu.memory.updated(x, cpu.ref(x) - cpu.ref(y)),
    )
  }

  // multiply x by y
  case class MUL(x: Reg, y: Ref) extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu.copy(
      pc = cpu.pc + 1,
      memory = cpu.memory.updated(x, cpu.ref(x) * cpu.ref(y)),
    )
  }

  // do nothing
  case object NOOP extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu.copy(pc = cpu.pc + 1)
  }

  case class OUT(x: Reg) extends Assembunny {
    override def execute(cpu: CPU): CPU = cpu.copy(pc = cpu.pc + 1, output = cpu.output ++ List(cpu.ref(x)))
  }

  def assembunny(s: String): Assembunny = s match {
    case Assembunny.cpy(x, y) => CPY(Ref(x), Reg(y))
    case Assembunny.inc(x) => INC(Reg(x))
    case Assembunny.dec(x) => DEC(Reg(x))
    case Assembunny.jnz(x, y) => JNZ(Ref(x), Ref(y))
    case Assembunny.tgl(x) => TGL(Ref(x))
    case Assembunny.add(x, y) => ADD(Reg(x), Ref(y))
    case Assembunny.sub(x, y) => SUB(Reg(x), Ref(y))
    case Assembunny.mul(x, y) => MUL(Reg(x), Ref(y))
    case Assembunny.out(x) => OUT(Reg(x))
    case Assembunny.noop() => NOOP
    case "noop" => NOOP
  }

  def run(mem: Map[Reg, Long], input: List[Assembunny], stop: (CPU) => Boolean = _ => false): CPU = {
    var cpu = CPU(0, mem, input, Nil)
    while (!stop(cpu)) {
      if (cpu.pc < 0 || cpu.pc >= cpu.code.length) return cpu
      else {
        cpu = cpu.code(cpu.pc.toInt).execute(cpu)
      }
    }
    cpu
  }
}
