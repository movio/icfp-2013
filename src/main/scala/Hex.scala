import scala.util.Random
import java.math.BigInteger

case class ActualProblemSolver() {

  import Hex._
  import Training._
  import remote.Remote._
  val programSize = 11
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



case class ProblemSolver() {

  import Hex._
  import Training._
  import remote.Remote._
  val programSize = 11
  var trainingData: Map[Input, Output] = Map.empty[Long, Long]
  var programs = List.empty[Lambda1]

  //def gen(ops: Set[String], size: Int): List[Lambda1] = List(Lambda1(Id("x"), Id("x")))

  def start = {
    val problem = getTrainer(size = Some(programSize), operators = Some(Set()))
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
      case "mismatch" ⇒
        println("======================= 😿 NEED TO TRY AGAIN =================================")
        trainingData += (guessResponse.values.map(vs ⇒ hexToLong(vs(0)) → hexToLong(vs(1))).get)
        // TODO

      case "error" ⇒
        println("======================= 👎 ERROR =================================")
    }

    println(guessRequest)
    println(guessResponse)
  }

}




object Training extends App {

  import remote.Remote._
  import Hex._
  import spray.json._

  //ActualProblemSolver().start

  type Input = Long
  type Output = Long

  def getTrainingData(testRequest: EvalRequest): Map[Input, Output] = {
    val resp: EvalResponse = eval(testRequest)
    require(resp.status == "ok", s"got negative response status: ${resp.status}")

    Map(
      ((testRequest.arguments map Hex.hexToLong) zip (resp.outputs.get map Hex.hexToLong)): _*
    )
    // should return input
  }

  def trainingRequest(problemId: String) = {

    def generateProblemSet: List[String] = {
      @scala.annotation.tailrec
      def generateRandomInput(expectedSize: Int, accumulated: Set[Long]): Set[Long] = {
        if (accumulated.size == expectedSize) accumulated
        else generateRandomInput(expectedSize, accumulated + Random.nextLong)
      }

      generateRandomInput(256, Set(Long.MaxValue, Long.MinValue, -2L, -1L, 0L, 1L, 2L, 0xffL, 0xff00000000000000L)).toList map Hex.longToHex
    }

    EvalRequest(
      id = Some(problemId),
      program = None,
      arguments = generateProblemSet
    )
  }

  //println(trainingRequest("5LbBzemOisIyfXSFW4a3quPc"))

  //ProblemSolver("5LbBzemOisIyfXSFW4a3quPc").start


  // careful this makes a request
  //println(eval(trainingRequest("5LbBzemOisIyfXSFW4a3quPc")))

}




object Hex {
  val sample = randomSample()

  def randomHexString() = "0x" + Random.nextLong.toHexString
  def randomSample() = (1 to 256).map(_ => randomHexString())
  def hexToLong(hex: String) = new BigInteger(hex.substring(2), 16).longValue()
  def longToHex(l: Long) = "0x" + longWrapper(l).toHexString

  //println(hexToLong("0xffffffffffffffff"))
  //println(longToHex(new BigInteger("ffffffffffffffff", 16).longValue()))
}
