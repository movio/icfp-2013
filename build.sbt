
name := "icfp"

organization := "teamkiwi"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

libraryDependencies +=  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0"

libraryDependencies += "io.spray" %%  "spray-json" % "1.2.5"

libraryDependencies += "io.spray" %%  "spray-json" % "1.2.5"

// Akka stuff
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.0"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.2.0"

libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.2.0"

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.2.0"

libraryDependencies += "com.typesafe" % "config" % "1.0.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.7"

