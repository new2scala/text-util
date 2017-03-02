package org.dele.misc.learnAkka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout
import concurrent.duration._
import akka.pattern.ask
/**
  * Created by dele on 2017-03-02.
  */
object TaskActors extends App {

  def log(str:String):Unit = {
    val n = System.nanoTime
    val diff = (n - startTime) / 1e+9f
    println(f"[$diff%.3f] $str")
  }

  import TaskStatus._
  import TaskWorkers._
  class TaskStatusActor(id:Int) extends Actor {
    var status:Option[String] = None
    val workerActor = context.actorOf(TaskWorkers.props(id, self))

    override def receive: Receive = {
      case StartTask => {
        workerActor ! RunTask
      }
      case UpdateStatus(newStatus) => {
        log(s"task $id new status: $newStatus")
        status = Option(newStatus)
      }
      case QueryStatus => {
        sender ! status
      }
    }
  }
  object TaskStatus {
    case object StartTask
    case class UpdateStatus(newStatus:String)
    case object QueryStatus
    def props(id:Int) = Props(new TaskStatusActor(id))
  }

  class TaskWorkerActor(val id:Int, val statusActor:ActorRef) extends Actor {

    //val statusActor = context.actorOf(TaskStatus.props(id))
    import context.dispatcher
    implicit val timeout = Timeout(1 second)

    override def receive: Receive = {
      case RunTask => {
        statusActor ! UpdateStatus("Started!")
        Thread.sleep(5000)
        statusActor ! UpdateStatus("Status1!")
        Thread.sleep(5000)
        statusActor ! UpdateStatus("Status2!")
      }
      /*
    case GetStatus => {
      val s = statusActor ? QueryStatus
      val r = Await.result(s, 5 seconds)
      log(s"await result: $r")
      sender ! r
    } */
    }
  }

  object TaskWorkers {
    case object RunTask
    def props(taskId:Int, statusActor:ActorRef) = Props(new TaskWorkerActor(taskId, statusActor))
  }

  import TaskManager._

  import collection.concurrent
  class TaskManagerActor extends Actor {
    private val taskMap = concurrent.TrieMap[Int, ActorRef]()
    implicit val timeout = Timeout(2 second)
    import context.dispatcher
    override def receive: Receive = {
      case QueryTask(taskId) => {
        if (taskMap.contains(taskId)) {
          val fut = taskMap(taskId) ? QueryStatus
          fut.onComplete { r =>
            log(s"result: $r")
          }
        }
      }
      case NewTask => {
        val newTaskId = taskMap.size+1
        val newTask = context.actorOf(TaskStatus.props(newTaskId))
        taskMap += newTaskId -> newTask
        log(s"new task $newTaskId created")
        newTask ! StartTask
        log(s"new task $newTaskId started")
      }
    }
  }

  object TaskManager {
    case class QueryTask(taskId:Int)
    case object NewTask
    def props() = Props(new TaskManagerActor())
  }

  val startTime = System.nanoTime
  val system = ActorSystem("task-system")
  val taskManger = system.actorOf(TaskManager.props())
  taskManger ! NewTask
  taskManger ! NewTask
  Thread.sleep(3000)
  taskManger ! QueryTask(1)
  Thread.sleep(4000)
  taskManger ! QueryTask(1)
  taskManger ! QueryTask(2)
  Thread.sleep(1000)
  taskManger ! QueryTask(1)
  taskManger ! QueryTask(2)
  Thread.sleep(4000)
  taskManger ! QueryTask(1)
  taskManger ! QueryTask(2)
  Thread.sleep(1000)
  system.terminate()
}
