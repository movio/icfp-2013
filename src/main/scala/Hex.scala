import scala.util.Random
import java.math.BigInteger


object Training extends App {

  import remote.Remote._
  import Hex._
  import spray.json._

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

  println(trainingRequest("5LbBzemOisIyfXSFW4a3quPc"))

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
