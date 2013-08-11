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

  def start = {
    TeamKiwi.fun map { problem ⇒
      println(s"got problem: $problem")
      solve(problem)
    }
  }

  def solve(problem: Problem, count: Int = 0): Unit = {
    trainingData ++= getTrainingData(trainingRequest(problem.id))
    println(s"first trainingData: ${trainingData.take(1)}")
    // step 3
    if (count == 0){
      programs = Generator.generateProblems(problem.operators, problem.size)
      println(s"first generated program: ${programs.take(1)}")
    }
    programs = programs.filter { p ⇒
      trainingData.map { case (i, o) ⇒
        val ir = Interpreter.eval(p, i).head
        ir == o
      }.forall(_ == true)
    }
    println(s"first program left after filter: ${programs.take(1)}")
    // step 4
    val solution: String = Pretty.stringify(programs.head)
    println(s"looking at solution: $solution")

    // step 5
    val guessRequest = GuessRequest(id = problem.id, program = solution)
    val guessResponse = guess(guessRequest)

    guessResponse.status match {
      case "win" ⇒
        println("======================= 👍 SOLVED =================================")
        trainingData = Map.empty[Long, Long]
        println("sleeping for 10s before next problem")
        Thread.sleep(10 * 1000)
      case "mismatch" ⇒
        println("======================= 😿 NEED TO TRY AGAIN =================================")
        trainingData += (guessResponse.values.map(vs ⇒ hexToLong(vs(0)) → hexToLong(vs(1))).get)
        println("sleeping for 10s before retry")
        Thread.sleep(10 * 1000)
        if (count > 2) throw new Exception("NEED TO TRY AGAIN")
        else solve(problem, count + 1)

      case "error" ⇒
        println("======================= 👎 ERROR =================================")
        throw new Exception("ERROR")
    }

    println(guessRequest)
    println(guessResponse)
  }

}
