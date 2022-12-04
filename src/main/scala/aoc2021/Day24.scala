package aoc2021

import aoc2018.Day17.fixedPoint

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day24 extends App {

  sealed trait Variable

  case object W extends Variable

  case object X extends Variable

  case object Y extends Variable

  case object Z extends Variable

  def variable(ch: Char): Variable = ch match {
    case 'w' => W
    case 'x' => X
    case 'y' => Y
    case 'z' => Z
  }

  sealed trait Argument

  case class VariableArgument(value: Variable) extends Argument

  case class ConstantArgument(value: Int) extends Argument

  def argument(s: String): Argument = s match {
    case "w" | "x" | "y" | "z" => VariableArgument(variable(s.head))
    case x => ConstantArgument(x.toInt)
  }

  sealed trait Instruction

  case class Inp(arg: Argument) extends Instruction

  case class Add(a: Argument, b: Argument) extends Instruction

  case class Mul(a: Argument, b: Argument) extends Instruction

  case class Div(a: Argument, b: Argument) extends Instruction

  case class Mod(a: Argument, b: Argument) extends Instruction

  case class Eql(a: Argument, b: Argument) extends Instruction

  val program: List[Instruction] = Using(Source.fromFile("inputs/2021/24.txt"))(_.getLines().map {
    case s"inp $a" => Inp(argument(a))
    case s"add $a $b" => Add(argument(a), argument(b))
    case s"mul $a $b" => Mul(argument(a), argument(b))
    case s"div $a $b" => Div(argument(a), argument(b))
    case s"mod $a $b" => Mod(argument(a), argument(b))
    case s"eql $a $b" => Eql(argument(a), argument(b))
  }.toList).get

  type Memory = Map[Variable, Long]

  def write(memory: Memory, arg: Argument, value: Long): Memory = {
    memory.updated(arg.asInstanceOf[VariableArgument].value, value)
  }

  def read(memory: Memory, arg: Argument): Long = arg match {
    case VariableArgument(variable) => memory(variable)
    case ConstantArgument(constant) => constant
  }

  def update(memory: Memory, a: Argument, b: Argument, op: (Long, Long) => Long): Memory = {
    write(memory, a, op(read(memory, a), read(memory, b)))
  }

  @tailrec
  def eval(program: List[Instruction], inputs: List[Int], memory: Memory): Memory = program match {
    case Inp(arg) :: rest => eval(rest, inputs.tail, write(memory, arg, inputs.head))
    case Add(a, b) :: rest => eval(rest, inputs, update(memory, a, b, _ + _))
    case Mul(a, b) :: rest => eval(rest, inputs, update(memory, a, b, _ * _))
    case Div(a, b) :: rest => eval(rest, inputs, update(memory, a, b, _ / _))
    case Mod(a, b) :: rest => eval(rest, inputs, update(memory, a, b, _ % _))
    case Eql(a, b) :: rest => eval(rest, inputs, update(memory, a, b, (a, b) => if (a == b) 1 else 0))
    case Nil => memory
  }

  def validate(modelNo: List[Int]): Long = {
    eval(program, modelNo, Map(W -> 0, X -> 0, Y -> 0, Z -> 0))(Z)
  }

  case class EvaluatedModelNo(modelNo: List[Int], value: Long)

  def lt(comparator: (List[Int], List[Int]) => Boolean)(e1: EvaluatedModelNo, e2: EvaluatedModelNo): Boolean = {
    val comparedValues = e1.value.compareTo(e2.value)
    if (comparedValues == 0) comparator(e1.modelNo, e2.modelNo)
    else comparedValues < 0
  }

  def improveWith(comparator: (List[Int], List[Int]) => Boolean)(exploder: List[Int] => List[List[Int]])(model: EvaluatedModelNo): EvaluatedModelNo = {
    exploder(model.modelNo)
      .map(c => EvaluatedModelNo(c, validate(c)))
      .filter(p => p.value <= model.value)
      .sortWith(lt(comparator))
      .headOption
      .getOrElse(model)
  }

  def explode0(candidate: List[Int], idx: Int): List[List[Int]] = {
    (1 to 9).map(candidate.updated(idx, _)).toList
  }

  def explode1(modelNo: List[Int]): List[List[Int]] = {
    for {
      i <- (0 until 14).toList
      a <- explode0(modelNo, i)
    } yield a
  }

  def explode2(modelNo: List[Int]): List[List[Int]] = {
    for {
      a <- explode1(modelNo)
      b <- explode1(a)
    } yield b
  }

  def findOptimum(comparator: (List[Int], List[Int]) => Boolean)(initial: List[Int]): String = {
    val firstApprox = fixedPoint(improveWith(comparator)(explode1))(EvaluatedModelNo(initial, validate(initial)))
    fixedPoint(improveWith(comparator)(explode2))(firstApprox).modelNo.mkString("")
  }

  def listComparator(elementComparator: (Int, Int) => Boolean)(e1: List[Int], e2: List[Int]): Boolean = {
    e1.lazyZip(e2).find(p => p._1 != p._2).exists(elementComparator.tupled)
  }

  println(findOptimum(listComparator(_ > _))(List.fill(14)(9)))

  println(findOptimum(listComparator(_ < _))(List.fill(14)(1)))
}
