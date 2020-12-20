package aoc2020

import scala.io.Source

object Day20 extends App {

  case class Tile(id: Long, pixels: Map[(Int, Int), Boolean]) {

    val maxX: Int = pixels.keys.map(_._1).max
    val maxY: Int = pixels.keys.map(_._2).max

    def rotate(): Tile = {
      copy(pixels = pixels.map {
        case ((x, y), v) => (maxY - y, x) -> v
      })
    }

    def rotateN(n: Int): Tile = {
      if (n == 0) this
      else this.rotate().rotateN(n - 1)
    }

    def flipH: Tile = copy(pixels = pixels.map {
      case ((x, y), v) => (maxX - x, y) -> v
    })

    def flipV: Tile = copy(pixels = pixels.map {
      case ((x, y), v) => (x, maxY - y) -> v
    })

    def viz(): Unit = {
      val maxIdx = pixels.keys.map(_._1).max
      for {y <- 0 to maxY} {
        for {x <- 0 to maxX} {
          if (pixels(x, y)) print('#')
          else print('.')
        }
        println()
      }
    }
  }

  val input = {
    val lines = Source.fromFile("inputs/2020/20.txt").getLines().mkString("\n").split("\n\n").toList

    def parseTile(tile: List[String]): Tile = {
      val re = """Tile (\d+):""".r
      val id = tile.head match {
        case re(id) => id.toInt
      }
      val pixels = tile.tail.zipWithIndex.flatMap { case (row, y) =>
        row.zipWithIndex.map {
          case ('#', x) => (x, y) -> true
          case ('.', x) => (x, y) -> false
        }
      }.toMap
      Tile(id, pixels)
    }

    lines.map(line => parseTile(line.split("\n").toList)).toSet
  }

  val maxIdx = input.head.pixels.keys.map(_._1).max

  def matchLeft(left: Tile, right: Tile): Boolean = {
    (0 to maxIdx).forall(y => left.pixels(maxIdx, y) == right.pixels(0, y))
  }

  def matchNorth(up: Tile, down: Tile): Boolean = {
    (0 to maxIdx).forall(x => up.pixels(x, maxIdx) == down.pixels(x, 0))
  }

  def neighs(leftNeigh: Option[Tile], northNeigh: Option[Tile], used: Set[Long]): Set[Tile] = {
    for {
      tile <- input
      if !used.contains(tile.id)
      flipped <- Set(tile, tile.flipH, tile.flipV)
      rotations <- 0 until 4
      rotatedTile = flipped.rotateN(rotations)
      if (leftNeigh.isEmpty || matchLeft(leftNeigh.get, rotatedTile))
      if (northNeigh.isEmpty || matchNorth(northNeigh.get, rotatedTile))
    } yield rotatedTile
  }

  val rows = math.round(math.sqrt(input.size)).toInt
  val cols = rows

  def seaMonster(tiles: Map[(Int, Int), Tile]): Unit = {
    val shifted = for {
      row <- 0 until rows
      col <- 0 until cols
      tile = tiles(row, col)
      offsetX = (maxIdx - 1) * col - 1
      offsetY = (maxIdx - 1) * row - 1
      shiftedPixels = tile.pixels.filter {
        case ((0, _), _) => false
        case ((_, 0), _) => false
        case ((`maxIdx`, _), _) => false
        case ((_, `maxIdx`), _) => false
        case _ => true
      }.map {
        case ((x, y), v) => (x + offsetX, y + offsetY) -> v
      }
    } yield shiftedPixels
    val assembled = shifted.foldLeft(Map[(Int, Int), Boolean]()) {
      case (acc, map) => map.foldLeft(acc) { case (z, (key, value)) =>
        assert(!z.contains(key))
        z.updated(key, value)
      }
    }
    val tile = Tile(-1, assembled)
    val seaMonster = Tile(-1, List(
      "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   ",
    ).zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.map {
        case ('#', x) => (x, y) -> true
        case (' ', x) => (x, y) -> false
      }
    }.toMap)
    val monsters = for {
      flipped <- Set(seaMonster, seaMonster.flipH, seaMonster.flipV)
      rotations <- 0 until 4
      rotatedMonster = flipped.rotateN(rotations)
    } yield rotatedMonster
    val monsterTiles = monsters.foldLeft(Set[(Int, Int)]()) {
      case (acc, monster) => acc ++ overlap(monster, tile)
    }
    val roughness = tile.pixels.count {
      case (x, true) if !(monsterTiles contains x) => true
      case _ => false
    }
    println(s"rougness = $roughness")
  }

  def overlap(monster: Tile, tile: Tile): Set[(Int, Int)] = {
    (for {
      x <- 0 to tile.maxX - monster.maxX
      y <- 0 to tile.maxY - monster.maxY
      shiftedMonster = monster.pixels.foldLeft(Set[(Int, Int)]()) {
        case (acc, ((mx, my), true)) => acc ++ Set((x + mx, y + my))
        case (acc, _) => acc
      }
      if (shiftedMonster.forall {
        case (mx, my) => tile.pixels.getOrElse((mx, my), false)
        case _ => false
      })
      monsterPixel <- shiftedMonster
    } yield monsterPixel).toSet
  }

  def solve(row: Int, col: Int, acc: Map[(Int, Int), Tile]): Set[Map[(Int, Int), Tile]] = {

    val leftNeigh = if (col > 0) Some(acc(row, col - 1)) else None
    val northNeigh = if (row > 0) Some(acc(row - 1, col)) else None
    val ns = neighs(leftNeigh, northNeigh, acc.values.map(_.id).toSet)

    if (acc.size == rows * cols) {
      val sol = acc
      seaMonster(sol)
      Set(acc)
    }
    else {
      for {
        n <- ns
        sol <- solve(
          if (col == cols - 1) row + 1 else row,
          if (col == cols - 1) 0 else col + 1,
          acc.updated((row, col), n))
      } yield sol
    }
  }

  val sol = solve(0, 0, Map()).head

  println(sol(0, 0).id * sol(0, cols - 1).id * sol(rows - 1, 0).id * sol(rows - 1, cols - 1).id)
}
