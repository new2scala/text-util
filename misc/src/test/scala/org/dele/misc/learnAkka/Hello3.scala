package org.dele.misc.learnAkka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

/**
  * Created by dele on 2017-03-01.
  */
object Hello3 extends App {

  import Greeter._
  class Greeter(myName: String, greeting: String) extends Actor {
    def receive = {
      case Greet(peer) => {
        println(s"[$myName] in Greet($peer)")
        peer ! AskName
      }
      case AskName => {
        println(s"[$myName] in AskName sender is: $sender")
        sender ! GreetName(myName)
      }
      case GreetName(name) => {
        println(s"[$myName] in GreetName($name)")
        println(s"$greeting, $name")
      }
    }
  }

  object Greeter {
    case class Greet(peer: ActorRef)
    case object AskName
    case class GreetName(name: String)
    def props(name: String, greeting: String) = Props(new Greeter(name, greeting))
  }

  val system = ActorSystem("actor-demo-2")
  val bob = system.actorOf(props("Bob", "Howya doing"))
  val alice = system.actorOf(props("Alice", "Nice to meet you"))
  bob ! Greet(alice)
  alice ! Greet(bob)

  Thread.sleep(1000)

  system.terminate()

}
