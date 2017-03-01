package org.dele.misc.learnAkka

import java.util.concurrent.Future

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout
import akka.pattern.ask

import scala.concurrent.duration._

/**
  * Created by dele on 2017-03-01.
  */
object Hello4 extends App {
  import Greeter._
  val system = ActorSystem("actor-demo-3")
  val bob = system.actorOf(props("Bob", "Howya doing"))
  val alice = system.actorOf(props("Alice", "Happy to meet you"))
  bob ! Greet(alice)
  alice ! Greet(bob)
  Thread sleep 1000
  system.terminate()

  class Greeter(myName:String, greeting:String) extends Actor {
    import system.dispatcher
    implicit val timeout = Timeout(5 seconds)
    override def receive: Receive = {
      case Greet(peer) => {
        val futureName = peer ? AskName
        futureName.foreach( n => println(s"$greeting, $n"))
      }
      case AskName => {
        sender ! myName
      }
    }
  }

  object Greeter {
    case class Greet(peer: ActorRef)
    case object AskName
    def props(name:String, greeting:String) = Props(new Greeter(name, greeting))
  }

}
