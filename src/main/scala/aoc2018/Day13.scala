package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {

  // TODO cleanup

  val (map, carts) = {
    val lines = Source.fromFile("inputs/2018/13.txt").getLines().toList
    val carts = for {
      y <- lines.indices
      x <- lines(y).indices
      ch = lines(y)(x)
      if Set('^', '<', 'v', '>') contains ch
    } yield Cart(x, y, ch, R)
    val map = for {
      line <- lines
      mapped = line.map {
        case '^' => '|'
        case '<' => '-'
        case 'v' => '|'
        case '>' => '-'
        case x => x
      }
    } yield mapped
    (map, carts.toList)
  }

  sealed trait Dir {
    def next: Dir
  }

  case object L extends Dir {
    def next: Dir = S
  }

  case object S extends Dir {
    def next: Dir = R
  }

  case object R extends Dir {
    def next: Dir = L
  }

  case class Cart(x: Int, y: Int, dir: Char, lastTurn: Dir) {

    def coords: (Int, Int) = (x, y)

    private def forward: Cart = dir match {
      case '^' => copy(y = y - 1)
      case '<' => copy(x = x - 1)
      case 'v' => copy(y = y + 1)
      case '>' => copy(x = x + 1)
    }

    private def turn: Cart = ((dir, lastTurn.next) match {
      case ('^', L) => copy(dir = '<')
      case ('<', L) => copy(dir = 'v')
      case ('v', L) => copy(dir = '>')
      case ('>', L) => copy(dir = '^')

      case (_, S) => this

      case ('^', R) => copy(dir = '>')
      case ('<', R) => copy(dir = '^')
      case ('v', R) => copy(dir = '<')
      case ('>', R) => copy(dir = 'v')
    }).copy(lastTurn = lastTurn.next)

    def move(map: List[String]): Cart = ((dir, map(y)(x)) match {
      case ('^', '\\') => copy(dir = '<')
      case ('<', '\\') => copy(dir = '^')
      case ('v', '\\') => copy(dir = '>')
      case ('>', '\\') => copy(dir = 'v')

      case ('^', '/') => copy(dir = '>')
      case ('<', '/') => copy(dir = 'v')
      case ('v', '/') => copy(dir = '<')
      case ('>', '/') => copy(dir = '^')

      case (_, '+') => turn
      case (_, _) => this
    }).forward
  }

  def sortCarts(carts: List[Cart]): List[Cart] = carts.sortBy(_.coords.swap)

  def p1() {

    @tailrec def moveAll(todo: List[Cart], moved: List[Cart]): Either[(Int, Int), List[Cart]] = {
      if (todo.isEmpty) Right(moved)
      else {
        val head = todo.head
        val newCart = head.move(map)
        val locs = (todo.tail ++ moved).map(_.coords).toSet
        if (locs.contains(newCart.x, newCart.y)) Left(newCart.coords)
        else moveAll(todo.tail, newCart :: moved)
      }
    }

    @tailrec def firstCollision(carts: List[Cart]): (Int, Int) = {
      val result = moveAll(sortCarts(carts), Nil)
      if (result.isLeft) result.left.get
      else firstCollision(result.right.get)
    }

    println(firstCollision(carts))
  }

  p1()

  def p2() {

    @tailrec def moveAll(todo: List[Cart], moved: List[Cart]): List[Cart] = {
      if (todo.isEmpty) moved
      else {
        val head = todo.head
        val newCart = head.move(map)
        val locs = (todo.tail ++ moved).map(_.coords).toSet
        if (locs.contains(newCart.coords)) moveAll(todo.tail.filter(cart => cart.coords != newCart.coords),
          moved.filter(cart => cart.coords != newCart.coords))
        else moveAll(todo.tail, newCart :: moved)
      }
    }

    @tailrec def lastRemaining(carts: List[Cart]): (Int, Int) = {
      if (carts.size == 1) (carts.head.coords)
      else lastRemaining(moveAll(sortCarts(carts), Nil))
    }

    println(lastRemaining(carts))
  }

  p2()
}
