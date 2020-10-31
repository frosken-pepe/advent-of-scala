package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day07 extends App {

  val step = """Step (\w) must be finished before step (\w) can begin.""".r

  case class WorkItem(id: Char) {
    def time: Int = 60 + (id - 'A' + 1)
  }

  object WorkItem {
    implicit val ordering: Ordering[WorkItem] = (x: WorkItem, y: WorkItem) => x.id.compareTo(y.id)
  }

  val steps = Source.fromFile("inputs/2018/07.txt").getLines()
    .map { case step(a, b) => (WorkItem(a.head), WorkItem(b.head)) }
    .toList

  val items = steps.flatMap(s => List(s._1, s._2)).distinct.sorted

  @tailrec def executeSteps(todo: List[WorkItem], done: List[WorkItem]): List[WorkItem] = {
    if (todo.isEmpty) done.reverse
    else {
      val canBegin = todo.find(r => steps.filter(_._2 == r).forall(q => done.contains(q._1))).get
      executeSteps(todo.filter(_ != canBegin), canBegin :: done)
    }
  }

  println(executeSteps(items, Nil).map(_.id).mkString)

  case class WorkInProgress(item: WorkItem, finishes: Int)

  case class Worker(id: Int, wip: Option[WorkInProgress])

  case class State(time: Int, todo: List[WorkItem], done: List[WorkItem], workers: List[Worker]) {
    def nextUnblockedItem: Option[WorkItem] = todo.find(r => steps.filter(_._2 == r).forall(q => done.contains(q._1)))
  }

  val initialState = State(0, items, Nil, (1 to 5).map(id => Worker(id, None)).toList)

  val des = LazyList.unfold(initialState) { state =>
    if (state.done.length == initialState.todo.length) None
    else if (state.workers.exists(_.wip.exists(_.finishes == state.time))) {
      val worker = state.workers.filter(_.wip.exists(_.finishes == state.time)).head
      val wip = worker.wip.get
      println(s"[${state.time}] worker ${worker.id} finishes work on ${wip.item.id}")
      Some(state.time, state.copy(
        workers = state.workers.map {
          case Worker(id, _) if id == worker.id => Worker(id, None)
          case w => w
        },
        done = wip.item :: state.done
      ))
    } else if (state.workers.exists(_.wip.isEmpty) && state.nextUnblockedItem.nonEmpty) {
      val worker = state.workers.filter(_.wip.isEmpty).head
      val work = state.nextUnblockedItem.get
      val newYorker = worker.copy(wip = Some(WorkInProgress(work, state.time + work.time)))
      println(s"[${state.time}] worker ${worker.id} starts work on ${work.id}")
      Some(state.time, state.copy(
        todo = state.todo.filter(_ != work),
        workers = state.workers.map {
          case Worker(id, _) if id == worker.id => newYorker
          case w => w
        }
      ))
    } else if (state.workers.exists(w => w.wip.nonEmpty)) {
      val nextTime = state.workers.flatMap(_.wip.map(_.finishes)).min
      Some(state.time, state.copy(time = nextTime))
    } else throw new IllegalStateException()
  }

  println(des.toList.last)
}
