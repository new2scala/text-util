package org.dele.misc.learnAkka
import akka.actor.Actor.Receive

import scala.concurrent.duration._
import scala.util.Random
import akka.actor._
import akka.util._
/**
  * Created by dele on 2017-03-01.
  */
object Stars1 extends App {
  val starBaseLifetime = 5000 millis
  val starVariableLifetime = 2000 millis
  val starBaseSpawntime = 2000 millis
  val starVariableSpawntime = 1000 millis

  val random = new Random(100)

  import Star._
  val namerPath = "/user/namer"
  val system = ActorSystem("stars-demo")
  val scheduler = system.scheduler
  system.actorOf(Namer.props(Array("Bob", "Alice", "Rock", "Paper", "Scissors",
    "North", "South", "East", "West", "Up", "Down")), "namer")
  val star1 = system.actorOf(props("Howya doing", 1, "Nobody"))
  val star2 = system.actorOf(props("Happy to meet you", 1, "Nobody"))

  Thread.sleep(500)
  star1 ! Greet(star2)

  class Namer(names:Array[String]) extends Actor {
    import Namer._
    import context.dispatcher

    context.setReceiveTimeout(starBaseSpawntime)

    def receive = {
      case GetName => {
        val nameIdx = math.abs(random.nextInt) % names.length
        val name = names(nameIdx)
        sender ! SetName(name)
      }
      case ReceiveTimeout => {
        println("Namer receive timeout")
        system.terminate()
      }
    }
  }

  class Star(greeting:String, genNum:Int, parent:String) extends Actor {
    var myName: String = ""
    var starsKnown = Map[String, ActorRef]()

    val namer = context.actorSelection(namerPath)
    namer ! Namer.GetName

    def scaledDuration(base: FiniteDuration, variable: FiniteDuration) =
      base + variable * random.nextInt(1000) / 1000

    import context.dispatcher
    val killtime = scaledDuration(starBaseLifetime, starVariableLifetime)
    val killer = scheduler.scheduleOnce(killtime, self, Die)
    val spawntime = scaledDuration(starBaseSpawntime, starVariableSpawntime)
    val spawner = scheduler.schedule(spawntime, 1 second, self, Spawn)
    if (genNum > 1) scheduler.scheduleOnce(1 second, context.parent, IntroduceMe)

    override def receive: Receive = {
      case Namer.SetName(name) => {
        myName = name
        println(s"$name is the ${genNum}th generation child of $parent")
        context become(named)
      }
    }

    def named: Receive = {
      case Greet(peer) => peer ! AskName
      case AskName => sender ! TellName(myName)
      case TellName(name) => {
        println(s"$myName says: '$greeting, $name'")
        starsKnown += name -> sender()
      }
      case Spawn => {
        println(s"$myName says: spawning a new star!")
        context.actorOf(props(greeting, genNum+1, myName))
      }
      case IntroduceMe => starsKnown.foreach{
        case (name, ref) => ref ! Greet(sender)
      }
      case Die => {
        println(s"$myName says: 'I'd like to thank the Academy...'")
        context stop self
      }
    }
  }

  object Star {
    case class Greet(peer:ActorRef)
    case object AskName
    case class TellName(name:String)
    case object Spawn
    case object IntroduceMe
    case object Die
    def props(greeting:String, genNum:Int, parent:String) =
      Props(new Star(greeting, genNum, parent))
  }

  object Namer {
    case object GetName
    case class SetName(name:String)
    def props(names: Array[String]):Props = Props(new Namer(names))
  }
}
