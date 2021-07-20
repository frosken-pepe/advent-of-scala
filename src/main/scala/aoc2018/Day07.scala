package aoc2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day07 extends App {

  val step = """Step (\w) must be finished before step (\w) can begin.""".r

  case class WorkItem(id: Char) {
    def time: Int = 60 + (id - 'A' + 1)
  }

  object WorkItem {
    implicit val ordering: Ordering[WorkItem] = (x: WorkItem, y: WorkItem) => x.id.compareTo(y.id)
  }

  val steps = Using(Source.fromFile("inputs/2018/07.txt"))(_.getLines()
    .map { case step(a, b) => (WorkItem(a.head), WorkItem(b.head)) }
    .toList).get

  val items = steps.flatMap(s => List(s._1, s._2)).distinct.sorted

  @tailrec def executeSteps(todo: List[WorkItem], done: List[WorkItem]): List[WorkItem] = {
    if (todo.isEmpty) done.reverse
    else {
      val canBegin = todo.find(r => steps.filter(_._2 == r).forall(q => done.contains(q._1))).get
      executeSteps(todo.filter(_ != canBegin), canBegin :: done)
    }
  }

  println(executeSteps(items, Nil).map(_.id).mkString)

  sealed trait WorkerState

  case object Idle extends WorkerState

  case class Working(item: WorkItem, finishes: Int) extends WorkerState

  case class Worker(id: Int, state: WorkerState) {

    def idle: Boolean = state match {
      case Idle => true
      case _ => false
    }

    def item: Option[WorkItem] = state match {
      case Working(item, _) => Some(item)
      case _ => None
    }

    def finishes: Option[Int] = state match {
      case Working(_, finishes) => Some(finishes)
      case _ => None
    }
  }

  case class State(time: Int, todo: List[WorkItem], done: List[WorkItem], workers: List[Worker]) {

    def next: Option[State] = (for {
      update <- finishWork #:: beginWork #:: advanceTime #:: LazyList.empty
    } yield update).flatten.headOption

    private def nextUnblockedItem: Option[WorkItem] = {
      todo.find(r => steps.filter(_._2 == r).forall(q => done.contains(q._1)))
    }

    private def advanceTime: Option[State] = (for {
      worker <- workers
      finishes <- worker.finishes
    } yield copy(time = finishes)).minByOption(_.time)

    private def beginWork: Option[State] = for {
      worker <- workers.find(_.idle)
      work <- nextUnblockedItem
    } yield copy(
      todo = todo.filter(_ != work),
      workers = updateWorkerState(worker.id, Working(work, time + work.time))
    )

    private def finishWork: Option[State] = (for {
      worker <- workers
      finishes <- worker.finishes if finishes == time
      item <- worker.item
    } yield copy(
      workers = updateWorkerState(worker.id, Idle),
      done = item :: done
    )).headOption

    private def updateWorkerState(workerId: Int, state: WorkerState) = workers.map {
      case w@Worker(id, _) if id == workerId => w.copy(state = state)
      case w => w
    }
  }

  val initialState = State(
    time = 0,
    todo = items,
    done = Nil,
    workers = (1 to 5).map(id => Worker(id, Idle)).toList
  )

  println(LazyList.unfold(initialState)(_.next.map(s => (s.time, s))).last)
}
