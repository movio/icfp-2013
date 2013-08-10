package remote

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.Props

class Master extends App {
  val customConf = ConfigFactory.parseString("""
      akka {
        event-handlers = ["akka.event.slf4j.Slf4jEventHandler"]
        loglevel = "DEBUG"

        remote {
		  enabled-transports = ["akka.remote.netty.tcp"]
		  netty.tcp {
		    port = 2552
		  }
		}
        actor {
		  provider = "akka.remote.RemoteActorRefProvider"
          deployment {
	        // This configuration setting will clone the actor “processor” 10 times
	        // and deploy it evenly distributed across the two given target nodes.

	        /db {
		        router = "round-robin"
		        nr-of-instances = 10
		         target {
		           nodes = ["akka.tcp://client@127.0.0.1:2553","akka.tcp://client@127.0.0.1:2554"]
		        }
		      }

	        /processor {
		        router = "round-robin"
		        nr-of-instances = 10
		         target {
		           nodes = ["akka.tcp://client@127.0.0.1:2553","akka.tcp://client@127.0.0.1:2554"]
		        }
		      }
           }
        }
      }
      """)

  val system = ActorSystem("master", ConfigFactory.load(customConf))

  val coordinator = system.actorOf(Props(new Coordinator), name = "coordinator")
  val sender = system.actorOf(Props(new Sender), name = "sender")


}