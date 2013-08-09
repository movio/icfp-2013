
name := "icfp"

organization := "teamkiwi"

version := "0.1.0-SNAPSHOT"
  
scalaVersion := "2.10.2"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

libraryDependencies +=  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0"

libraryDependencies += "io.spray" %%  "spray-json" % "1.2.5"


