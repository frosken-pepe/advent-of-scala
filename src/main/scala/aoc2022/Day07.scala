package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day07 extends App {

  private sealed trait Content

  private case class File(name: String, size: Long) extends Content

  private case class Dir(name: String) extends Content

  private sealed trait Command

  private case class Ls(output: List[Content]) extends Command

  private case class Cd(target: String) extends Command

  private case class Output(content: Content) extends Command

  private def command(s: String): Command = s match {
    case s"$x cd $target" if x == "$" => Cd(target)
    case s"$x ls" if x == "$" => Ls(Nil)
    case s"dir $dirname" => Output(Dir(dirname))
    case s"$size $fileName" => Output(File(fileName, size.toInt))
  }

  private val session = Using(Source.fromFile("inputs/2022/07.txt"))(_.getLines().toList)
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

  private case class Directory(size: Long, subdirs: List[String])

  @tailrec
  private def scan(output: List[Content], dir: Directory): Directory = {
    if (output.isEmpty) dir
    else output.head match {
      case File(_, size) => scan(output.tail, dir.copy(size = dir.size + size))
      case Dir(name) => scan(output.tail, dir.copy(subdirs = name :: dir.subdirs))
    }
  }

  private def changeDir(target: String, currentDir: List[String]) = target match {
    case "/" => Nil
    case ".." => currentDir.tail
    case x => x :: currentDir
  }

  @tailrec
  private def traverse(todo: List[Command], currentDir: List[String], directories: Map[List[String], Directory]): Map[List[String], Directory] = {
    if (todo.isEmpty) directories
    else todo.head match {
      case Ls(output) =>
        val scanned = scan(output, Directory(0, Nil))
        traverse(todo.tail, currentDir, directories.updated(currentDir, scanned))
      case Cd(target) =>
        traverse(todo.tail, changeDir(target, currentDir), directories)
      case Output(_) => throw new IllegalArgumentException()
    }
  }

  private val dirs: Map[List[String], Directory] = traverse(session, Nil, Map())

  private def totalSize(dir: List[String]): Long = {
    @tailrec
    def go(todo: List[List[String]], acc: Long): Long = {
      if (todo.isEmpty) acc
      else {
        val cur = todo.head
        val subdirs = dirs(cur).subdirs.map(sub => sub :: cur)
        go(subdirs ++ todo.tail, acc + dirs(cur).size)
      }
    }

    go(List(dir), 0)
  }

  println(dirs.keys.toList.map(totalSize).filter(_ <= 100000).sum)

  private val totalSpace = 70000000
  private val spaceNeeded = 30000000
  private val used = totalSize(Nil)
  private val unused = totalSpace - used
  private val needToRemove = spaceNeeded - unused

  println(dirs.keys.toList.map(totalSize).filter(_ >= needToRemove).min)
}
