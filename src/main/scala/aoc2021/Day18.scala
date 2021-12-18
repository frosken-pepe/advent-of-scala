package aoc2021

import play.api.libs.json._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day18 extends App {

  val input = Using(Source.fromFile("inputs/2021/18.txt"))(_.getLines()
    .toList).get

  sealed trait SNumber {
    val isLiteral: Boolean

    def deepcopy(): SNumber
  }

  object SNumber {
    def fromString(s: String): SNumber = {
      def go(json: JsValue): SNumber = {
        json match {
          case JsNumber(value) => SLiteral(value.intValue)
          case JsArray(value) => value.toList match {
            case a :: b :: Nil => SPair(go(a), go(b))
          }
        }
      }

      go(Json.parse(s))
    }
  }

  case class SLiteral(var value: Int) extends SNumber {
    override val isLiteral: Boolean = true

    override def deepcopy(): SNumber = SLiteral(value)

    override def toString: String = s"$value"
  }

  case class SPair(var left: SNumber, var right: SNumber) extends SNumber {
    override def toString: String = "[" + left + "," + right + "]"

    override def deepcopy(): SNumber = SPair(left.deepcopy(), right.deepcopy())

    override val isLiteral: Boolean = false
  }

  def flatten(s: SNumber, acc: List[SLiteral] = Nil): List[SLiteral] = {
    var list: List[SLiteral] = Nil

    def go(s: SNumber): Unit = {
      s match {
        case s@SLiteral(_) => list = list ++ List(s)
        case SPair(left, right) =>
          go(left)
          go(right)
      }
    }

    go(s)
    list
  }

  def explode(n: SNumber): Boolean = {
    val flattened = flatten(n)

    def go(cur: SPair, parent: SPair, isLeft: Boolean, level: Int): Boolean = {
      cur match {
        case SPair(left, right) if level == 4 => {
          val idxLeft = flattened.zipWithIndex.find(_._1 eq left).map(_._2 - 1).filter(_ >= 0)
          idxLeft.foreach(idx => flattened(idx).value += left.asInstanceOf[SLiteral].value)
          val idxRight = flattened.zipWithIndex.find(_._1 eq right).map(_._2 + 1).filter(_ < flattened.length)
          idxRight.foreach(idx => flattened(idx).value += right.asInstanceOf[SLiteral].value)
          if (isLeft) parent.left = SLiteral(0)
          else parent.right = SLiteral(0)
          true
        }
        case SPair(left, right) if level < 4 => {
          (left match {
            case pair: SPair => go(pair, cur, isLeft = true, level + 1)
            case _ => false
          }) || (right match {
            case pair: SPair => go(pair, cur, isLeft = false, level + 1)
            case _ => false
          })
        }
        case _ => false
      }
    }

    val res = go(n.asInstanceOf[SPair], null, isLeft = false, 0)
    println(s"after explode $n")
    res
  }

  def split(n: SNumber): Boolean = {
    def go(cur: SNumber, parent: SPair, isLeft: Boolean): Boolean = {
      cur match {
        case SLiteral(v) if v >= 10 => {
          val newPair = SPair(SLiteral(v / 2), SLiteral(v / 2 + (if (v % 2 == 1) 1 else 0)))
          if (isLeft) parent.left = newPair
          else parent.right = newPair
          true
        }
        case _: SLiteral => false
        case pair: SPair => {
          go(pair.left, pair, isLeft = true) || go(pair.right, pair, isLeft = false)
        }
      }
    }

    val res = go(n.asInstanceOf[SPair], null, isLeft = false)
    println(s"after split $n")
    res
  }

  @tailrec def reduce(s: SNumber): Boolean = {
    if (explode(s)) reduce(s)
    else if (split(s)) reduce(s)
    else false
  }

  def add(a: SNumber, b: SNumber): SNumber = {
    val temp = SPair(a.deepcopy(), b.deepcopy())
    reduce(temp)
    temp
  }

  def addAll(numbers: List[SNumber]): SNumber = {
    if (numbers.isEmpty) throw new IllegalArgumentException()
    else {
      numbers.tail.foldLeft(numbers.head) {
        case (a, b) => add(a, b)
      }
    }
  }

  def magnitude(number: SNumber): Int = {
    number match {
      case SLiteral(value) => value
      case SPair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
    }
  }

  val inputs = input.map(SNumber.fromString)

  println(magnitude(addAll(inputs)))

  println(
    (for {
      a <- inputs
      b <- inputs if a ne b
      mag = magnitude(add(a, b))
    } yield mag).max)
}
