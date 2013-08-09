
import dispatch._
import dispatch.Defaults._
import spray.json._
import spray.json.DefaultJsonProtocol._

object Remote extends App {
  val BaseUrl = "http://icfpc2013.cloudapp.net/"
  val params = Map("auth" -> "0328SAmjHevv7SW0OT2lYdIxjSuhVp2HX31j1dSSvpsH1H")

  case class TrainingRequest(
    size: Option[Int],
    operators: Option[Seq[String]])

  case class TrainingProblem(
    challenge: String,
    id: String,
    size: Int,
    operators: Seq[String])

  def getTrainer(size: Option[Int] = None, operators: Option[Seq[String]] = None): TrainingProblem =
    getTrainer(TrainingRequest(size, operators))

  def getTrainer(request: TrainingRequest): TrainingProblem = {
    implicit val requstFormatter = jsonFormat2(TrainingRequest)
    implicit val resultFormatter = jsonFormat4(TrainingProblem)

    val requestJson = request.toJson.compactPrint
    val result = Http(url(BaseUrl + "train").POST <<? params << requestJson OK as.String)

    result().asJson.convertTo[TrainingProblem]
  }

  case class Problem(
    id: String,
    size: Int,
    operators: Seq[String],
    solved: Option[Boolean],
    timeLeft: Option[Int])

  def myProblems(): Problem = {
    ???
  }

  case class EvalRequest(
    id: Option[String],
    program: Option[String],
    arguments: Seq[String])

  case class EvalResponse(
    status: String,
    outputs: Option[Seq[String]],
    message: Option[String])

  def eval(request: EvalRequest): EvalResponse = {
    ???
  }

  case class GuessRequest(
    id: String,
    program: String)

  case class GuessResponse(
    status: String,
    values: Option[Seq[String]],
    message: Option[String],
    lightning: Option[Boolean])

  def guess(request: EvalRequest): EvalResponse = {
    ???
  }

}

