package aoc2020

import scala.collection.mutable
import scala.io.Source

object Day18 extends App {

  val input = Source.fromFile("inputs/2020/18.txt").getLines().toList

  type Parser[A] = String => Option[(A, String)]

  sealed trait Expr {
    def eval: Long
  }

  case class Number(no: Int) extends Expr {
    override def eval: Long = no
  }

  case class Plus(left: Expr, right: Expr) extends Expr {
    override def eval: Long = left.eval + right.eval
  }

  case class Times(left: Expr, right: Expr) extends Expr {
    override def eval: Long = left.eval * right.eval
  }

  def char(ch: Char)(s: String): Option[(Char, String)] = {
    if (s.nonEmpty && s.head == ch) Some(ch, s.tail)
    else None
  }

  def number(s: String): Option[(Number, String)] = {
    if (s.nonEmpty && s.head.isDigit) {
      val (num, rest) = s.span(_.isDigit)
      Some((Number(num.toInt), rest))
    } else None
  }

  def parExpr(s: String): Option[(Expr, String)] = {
    for {
      (_, r0) <- char('(')(s)
      (exp, r1) <- expr(r0)
      (_, r2) <- char(')')(r1)
    } yield (exp, r2)
  }

  def either[A](a: Parser[A], b: Parser[A]): Parser[A] = { s =>
    a(s).orElse(b(s))
  }

  def term(s: String): Option[(Expr, String)] = {
    either(number, parExpr)(s)
  }

  def expr(s: String): Option[(Expr, String)] = {
    for {
      (left, r0) <- term(s)
      (ch, r1) <- either(char('+'), char('*'))(r0)
      (right, r2) <- either(expr, term)(r1)
    } yield if (ch == '+') (Plus(left, right), r2) else (Times(left, right), r2)
  }

  def reverseExpression(expr: String): String = {
    val tokens = expr.replace("(", "( ").replace(")", " )").split(" ")
    tokens.reverse.mkString("").map {
      case '(' => ')'
      case ')' => '('
      case x => x
    }
  }

  def evalExpression(e: String): Long =
    expr(reverseExpression(e)).map(_._1.eval).getOrElse(throw new IllegalStateException(s"for $e"))

  println(input.map(evalExpression).sum)

  def shuntingYard(s: String): List[String] = {

    def readToken(rest: String): (String, String) = {
      if (rest.head.isDigit) rest.span(_.isDigit)
      else (rest.head.toString, rest.tail)
    }

    var rest = s.replace(" ", "")
    val output = mutable.Queue[String]()
    val ops = mutable.Stack[String]()

    while (rest.nonEmpty) {
      val (token, newRest) = readToken(rest)
      rest = newRest
      if (token.head.isDigit) {
        output.enqueue(token)
      } else if (token == "+" || token == "*") {
        while (ops.nonEmpty && (ops.top == "+" && token == "*") && (ops.top != "(")) output.enqueue(ops.pop())
        ops.push(token)
      } else if (token == "(") {
        ops.push(token)
      } else if (token == ")") {
        while (ops.nonEmpty && ops.top != "(") {
          output.enqueue(ops.pop)
        }
        if (ops.nonEmpty && ops.top == "(") ops.pop
      }
    }
    while (ops.nonEmpty) output.enqueue(ops.pop)
    output.toList
  }

  def evalRPN(rpn: List[String]): Long = {
    val tokens = mutable.Stack[Long]()
    for (token <- rpn) {
      if (token.head.isDigit) tokens.push(token.toLong)
      else if (token == "*") tokens.push(tokens.pop * tokens.pop)
      else if (token == "+") tokens.push(tokens.pop + tokens.pop)
      else throw new IllegalStateException(s"token $token")
    }
    tokens.top
  }

  println(input.map(shuntingYard).map(evalRPN).sum)
}
