package remote

import akka.actor.ActorSystem

class Client extends App {
  val system = ActorSystem("client")
}