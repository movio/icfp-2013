import scala.util.Random
import java.math.BigInteger


object Hex {
  val sample = randomSample()

  def randomHexString() = "0x" + Random.nextLong.toHexString
  def randomSample() = (1 to 256).map(_ => randomHexString())
  def hexToLong(hex: String) = new BigInteger(hex.substring(2), 16).longValue()
  def longToHex(l: Long) = "0x" + longWrapper(l).toHexString

  //println(hexToLong("0xffffffffffffffff"))
  //println(longToHex(new BigInteger("ffffffffffffffff", 16).longValue()))
}
