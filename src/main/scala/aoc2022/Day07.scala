package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day07 extends App {

  sealed trait Content

  case class File(name: String, size: Long) extends Content

  case class Dir(name: String) extends Content

  sealed trait Command

  case class Ls(output: List[Content]) extends Command

  case class Cd(target: String) extends Command

  case class Output(content: Content) extends Command

  def command(s: String): Command = s match {
    case s"$x cd $target" if x == "$" => Cd(target)
    case s"$x ls" if x == "$" => Ls(Nil)
    case s"dir $dirname" => Output(Dir(dirname))
    case s"$size $fileName" => Output(File(fileName, size.toInt))
  }

  val session = Using(Source.fromFile("inputs/2022/07.txt"))(_.getLines().toList)
    .get
    .map(command)
    .foldLeft(List.empty[Command]) {
      case (acc, ls@Ls(_)) => ls :: acc
      case (acc, cd@Cd(_)) => cd :: acc
      case (acc, Output(content)) =>
        val ls = acc.head.asInstanceOf[Ls]
        ls.copy(output = content :: ls.output) :: acc.tail
    }
    .reverse

  case class Directory(size: Long, subdirs: List[String])

  @tailrec
  def scan(output: List[Content], dir: Directory): Directory = {
    if (output.isEmpty) dir
    else output.head match {
      case File(_, size) => scan(output.tail, dir.copy(size = dir.size + size))
      case Dir(name) => scan(output.tail, dir.copy(subdirs = name :: dir.subdirs))
    }
  }

  case class Path(parts: List[String]) {
    def cd(path: String): Path = path match {
      case "/" => Path(Nil)
      case ".." => Path(parts.tail)
      case x => Path(x :: parts)
    }
  }

  @tailrec
  def traverse(todo: List[Command], currentDir: Path, directories: Map[Path, Directory]): Map[Path, Directory] = {
    if (todo.isEmpty) directories
    else todo.head match {
      case Ls(output) =>
        val scanned = scan(output, Directory(0, Nil))
        traverse(todo.tail, currentDir, directories.updated(currentDir, scanned))
      case Cd(target) =>
        traverse(todo.tail, currentDir.cd(target), directories)
      case Output(_) => throw new IllegalArgumentException()
    }
  }

  val dirs: Map[Path, Directory] = traverse(session, Path(Nil), Map())

  def totalSize(dir: Path): Long = {
    @tailrec
    def go(todo: List[Path], acc: Long): Long = todo match {
      case Nil => acc
      case cur :: tail => go(dirs(cur).subdirs.map(cur.cd) ++ tail, acc + dirs(cur).size)
    }

    go(List(dir), 0)
  }

  println(dirs.keys.toList.map(totalSize).filter(_ <= 100000).sum)

  val totalSpace = 70000000
  val spaceNeeded = 30000000
  val used = totalSize(Path(Nil))
  val unused = totalSpace - used
  val needToRemove = spaceNeeded - unused

  println(dirs.keys.toList.map(totalSize).filter(_ >= needToRemove).min)
}
