package aoc2020

import scala.collection.immutable.Queue
import scala.io.Source

object Day18 extends App {

  val input = Source.fromFile("inputs/2020/18.txt").getLines().toList

  type Parser[A] = String => Option[(A, String)]

  sealed trait Token {
    def isOperator: Boolean
  }

  def evalRPN(precedence: Token => Boolean)(todo: List[Token], stack: List[Long] = Nil): Long = {
    if (todo.isEmpty) stack.head
    else {
      todo.head match {
        case Number(no) =>
          evalRPN(precedence)(todo.tail, no :: stack)
        case Operator('+') =>
          evalRPN(precedence)(todo.tail, (stack.head + stack.tail.head) :: stack.drop(2))
        case Operator('*') =>
          evalRPN(precedence)(todo.tail, (stack.head * stack.tail.head) :: stack.drop(2))
        case i@InfixExpression(_) =>
          evalRPN(precedence)(todo.tail, evalRPN(precedence)(i.toRPN(precedence)(), Nil) :: stack)
        case p@ParenthesizedExpression(_) =>
          evalRPN(precedence)(todo.tail, evalRPN(precedence)(p.subExpr.toRPN(precedence)()) :: stack)
      }
    }
  }

  case class Number(no: Int) extends Token {
    override def isOperator: Boolean = false

    override def toString: String = no.toString
  }

  case class Operator(to: Char) extends Token {
    override def isOperator: Boolean = true

    override def toString: String = to.toString
  }

  case class ParenthesizedExpression(subExpr: InfixExpression) extends Token {
    override def isOperator: Boolean = false

    override def toString: String = s"($subExpr)"
  }


  case class InfixExpression(tokens: List[Token]) extends Token {
    def toRPN(precedence: Token => Boolean)(todo: List[Token] = tokens, ops: List[Token] = Nil, out: Queue[Token] = Queue()): List[Token] = {
      if (todo.isEmpty && ops.nonEmpty) toRPN(precedence)(Nil, ops.tail, out.enqueue(ops.head))
      else if (todo.isEmpty) out.toList
      else todo.head match {
        case n@Number(_) => toRPN(precedence)(todo.tail, ops, out.enqueue(n))
        case p@ParenthesizedExpression(_) => toRPN(precedence)(todo.tail, ops, out.enqueue(p))
        case i@InfixExpression(_) => toRPN(precedence)(todo.tail, ops, out.enqueue(i))
        case Operator(_) if ops.nonEmpty && ops.head.isOperator && precedence(ops.head) =>
          toRPN(precedence)(todo, ops.tail, out.enqueue(ops.head))
        case Operator(_) => toRPN(precedence)(todo.tail, todo.head :: ops, out)
      }
    }

    override def isOperator: Boolean = false

    override def toString: String = s"${tokens.mkString(", ")}"
  }

  def char(ch: Char)(s: String): Option[(Char, String)] = {
    val t = s.dropWhile(_ == ' ')
    if (t.nonEmpty && t.head == ch) Some(ch, t.tail)
    else None
  }

  def number(s: String): Option[(Number, String)] = {
    val t = s.dropWhile(_ == ' ')
    if (t.nonEmpty && t.head.isDigit) {
      val (num, rest) = t.span(_.isDigit)
      Some((Number(num.toInt), rest))
    } else None
  }

  def parExpr(s: String): Option[(ParenthesizedExpression, String)] = {
    for {
      (_, r0) <- char('(')(s)
      (exp, r1) <- seq(r0)
      (_, r2) <- char(')')(r1)
    } yield (ParenthesizedExpression(exp), r2)
  }

  def either[A](a: Parser[A], b: Parser[A]): Parser[A] = { s =>
    a(s).orElse(b(s))
  }

  def term(s: String): Option[(Token, String)] = {
    either(number, parExpr)(s)
  }

  def seq(s: String): Option[(InfixExpression, String)] = {
    for {
      (first, r0) <- term(s)
      (ch, r1) <- either(char('+'), char('*'))(r0)
      (rest, r2) <- either(seq, term)(r1)
    } yield rest match {
      case n@Number(_) => (InfixExpression(List(first, Operator(ch), n)), r2)
      case p@ParenthesizedExpression(_) => (InfixExpression(List(first, Operator(ch), p)), r2)
      case InfixExpression(tokens) => (InfixExpression(List(first, Operator(ch)) ++ tokens), r2)
    }
  }

  def sumExpressions(precedence: Token => Boolean): Long = {
    input.map(seq).map(_.get).map(z => evalRPN(precedence)(z._1.toRPN(precedence)())).sum
  }

  println(sumExpressions(_ => true))
  println(sumExpressions {
    case Operator('+') => true
    case _ => false
  })
}
