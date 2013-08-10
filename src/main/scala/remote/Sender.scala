package remote

import akka.actor.Actor
import scala.concurrent.duration._
import scala.collection.mutable
import akka.actor.ActorLogging

class Sender extends Actor with ActorLogging {
  import Remote._

  val pending = new mutable.Queue[Request]

  def receive = {
    case Process ⇒ nextRequest()
    case r: Request ⇒ {
      pending += r
      log.debug(s"Queued $r, current queue size ${pending.size}")
    }
  }

  def nextRequest(): Unit = {
    val request = pending dequeue
    // TODO - fire off the request

    // What to do in failure
  }

  implicit val ec = context.dispatcher
  context.system.scheduler.schedule(0 seconds, 5 seconds) {
    self ! Process
  }

  case object Process
}