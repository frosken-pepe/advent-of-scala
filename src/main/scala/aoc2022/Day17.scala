package aoc2022

import scala.io.Source
import scala.util.Using

object Day17 extends App {

  case class Piece(rock: Set[(Long, Long)], width: Int, height: Int) {
    def shift(x: Long, y: Long): Piece = copy(rock = rock.map { case (xx, yy) => (x + xx, y + yy) })
  }

  def piece(ls: List[String]): Piece = {
    Piece(
      rock = ls.zipWithIndex.flatMap {
        case (str, row) => str.zipWithIndex.flatMap {
          case (ch, col) if ch == '#' => Some(col.toLong, (ls.length - row - 1).toLong)
          case _ => None
        }
      }.toSet,
      width = ls.head.length,
      height = ls.length,
    )
  }

  val pieces = List(
    List(
      "####",
    ),
    List(
      ".#.",
      "###",
      ".#.",
    ),
    List(
      "..#",
      "..#",
      "###",
    ),
    List(
      "#",
      "#",
      "#",
      "#",
    ),
    List(
      "##",
      "##",
    )
  ).map(piece)

  sealed trait Dir

  case object Left extends Dir

  case object Right extends Dir

  def dir(ch: Char): Dir = ch match {
    case '<' => Left
    case '>' => Right
  }

  val input: List[Dir] = Using(Source.fromFile("inputs/2022/17.txt"))(_.getLines().next().map(dir).toList).get

  case class Field(currentPiece: Option[Piece], rock: Set[(Long, Long)], maxY: Long, spawnCounter: Int, moveCounter: Int)


  val width = 7

  def spawn(field: Field): Field = {
    val template = pieces(field.spawnCounter)
    val piece = template.shift(2, field.maxY + 3 + 1)
    field.copy(currentPiece = Some(piece), spawnCounter = (field.spawnCounter + 1) % pieces.size)
  }

  def moveSideways(field: Field): Field = {
    val dir = input(field.moveCounter)
    val shifted = (field.currentPiece.get, dir) match {
      case (piece, Left) => piece.shift(-1, 0)
      case (piece, Right) => piece.shift(1, 0)
    }
    val blocked = shifted.rock.exists(_._1 < 0) || shifted.rock.exists(_._1 >= width) || shifted.rock.exists(field.rock)
    (if (blocked) field else field.copy(currentPiece = Some(shifted))).copy(moveCounter = (field.moveCounter + 1) % input.size)
  }

  def fall(field: Field): Field = {
    val shifted = field.currentPiece.get.shift(0, -1)
    val blocked = shifted.rock.exists(field.rock) || shifted.rock.exists(_._2 < 0)
    if (blocked) field.copy(rock = field.rock ++ field.currentPiece.get.rock, currentPiece = None, maxY = math.max(field.maxY, field.currentPiece.get.rock.map(_._2).max))
    else field.copy(currentPiece = Some(shifted))
  }

  def step(field: Field): Field = {
    field.currentPiece match {
      case Some(_) => fall(moveSideways(field))
      case _ => field
    }
  }

  def piece(field: Field): Field = {
    LazyList.iterate(spawn(field))(step).dropWhile(_.currentPiece.isDefined).head
  }

  val field = Field(None, Set.empty, -1, 0, 0)

  val ll = LazyList.iterate(field)(piece)

  def heightAfter(pieceNo: Long): Long = {
    ll.drop(pieceNo.toInt).head.maxY + 1L
  }

  println(heightAfter(2022))

  def isPeriodic(factor: Long): Boolean = {
    val heights = List(1, 2, 3, 4, 5).map(x => heightAfter(factor * x))
    heights.zip(heights.drop(1)).map {
      case (x, y) => y - x
    }.toSet.size == 1
  }

  val periodicity = LazyList.iterate(1L)(_ + 1).find(isPeriodic).head

  val delta = heightAfter(2 * periodicity) - heightAfter(periodicity)

  val numPieces = 1000000000000L

  println((numPieces / periodicity) * delta + heightAfter(numPieces % periodicity))
}
