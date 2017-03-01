package org.dele.misc.learnAkka

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorSystem, Props}

/**
  * Created by dele on 2017-03-01.
  */
object Hello extends App {
  case class Greeting(greeting:String)
  case class Greet(name:String)
  class HelloActor extends Actor {
    var _greeting = ""
    override def receive: Receive = {
      case Greeting(greeting) => _greeting = greeting
      case name: String => println(s"${_greeting} $name")
    }
  }
  val system = ActorSystem("actor-demo-1")

  val hello = system.actorOf(Props[HelloActor])

  hello ! "bob"
  hello ! Greeting("Hello")
  hello ! "bob"
  hello ! "alice"
  hello ! Greeting("Halo")
  hello ! "bob"
  hello ! "alice"

  Thread.sleep(1000)

  system.terminate()
}
