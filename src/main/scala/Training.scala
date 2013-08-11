import scala.util.Random

object Training extends App {

  import remote.Remote._
  import Hex._
  import spray.json._

  ProblemSolver().start

  type Input = Long
  type Output = Long

  def getTrainingData(testRequest: EvalRequest): Map[Input, Output] = {
    val resp: EvalResponse = eval(testRequest)
    require(resp.status == "ok", s"got negative response status: ${resp.status}")

    Map(
      ((testRequest.arguments map Hex.hexToLong) zip (resp.outputs.get map Hex.hexToLong)): _*
    )
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

}
