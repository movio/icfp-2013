object Solver extends App {

  import remote.Remote._
  import Hex._
  import spray.json._

  ActualProblemSolver().start
}


case class ActualProblemSolver() {

  import Hex._
  import Training._
  import remote.Remote._
  var trainingData: Map[Input, Output] = Map.empty[Long, Long]
  var programs = List.empty[Lambda1]
  val problems = TeamKiwi.fun

  def start = {
    problems map { problem ⇒
      println(s"got problem: $problem")

      trainingData = getTrainingData(trainingRequest(problem.id))
      println(s"got trainingData: $trainingData")

      // step 3
      programs = Generator.generateProblems(problem.operators, problem.size)
      println(s"generated programs: ${programs.take(5)}")
      programs = programs.filter { p ⇒
        trainingData.map { case (i, o) ⇒
          val ir = Interpreter.eval(p, i).head
          //println(s"got $i → $ir [$o] for program $p")
          ir == o
        }.forall(_ == true)
      }
      println(s"generated programs left after filter: ${programs.take(5)}")
      // step 4
      val solution: String = Pretty.stringify(programs.head)
      println(s"looking at solution: $solution")

      // step 5
      val guessRequest = GuessRequest(id = problem.id, program = solution)
      val guessResponse = guess(guessRequest)

      guessResponse.status match {
        case "win" ⇒
          println("======================= 👍 SOLVED =================================")
          println("SHOULD BE SLEEEPING")
          Thread.sleep(10 * 1000)
        case "mismatch" ⇒
          println("======================= 😿 NEED TO TRY AGAIN =================================")
          trainingData += (guessResponse.values.map(vs ⇒ hexToLong(vs(0)) → hexToLong(vs(1))).get)
          // TODO
          throw new Exception("NEED TO TRY AGAIN")

        case "error" ⇒
          println("======================= 👎 ERROR =================================")
          throw new Exception("ERROR")
      }

      println(guessRequest)
      println(guessResponse)
    }
  }

}
