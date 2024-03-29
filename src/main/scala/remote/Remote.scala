package remote

import dispatch._
import dispatch.Defaults._
import spray.json._
import spray.json.DefaultJsonProtocol._

object Remote {

  val BaseUrl = "http://icfpc2013.cloudapp.net/"
  val Params = Map("auth" -> "0328SAmjHevv7SW0OT2lYdIxjSuhVp2HX31j1dSSvpsH1H")

  case class TrainingRequest(
    size: Option[Int],
    operators: Option[Set[String]]) extends Request

  case class TrainingProblem(
    challenge: String,
    id: String,
    size: Int,
    operators: Set[String])

  def getTrainer(size: Option[Int] = None, operators: Option[Set[String]] = None): TrainingProblem =
    getTrainer(TrainingRequest(size, operators))

  def getTrainer(request: TrainingRequest): TrainingProblem = {
    implicit val requestFormatter = jsonFormat2(TrainingRequest)
    implicit val resultFormatter = jsonFormat4(TrainingProblem)

    val requestJson = request.toJson.compactPrint
    val result = Http(url(BaseUrl + "train").POST <<? Params << requestJson OK as.String)

    result().asJson.convertTo[TrainingProblem]
  }

  trait Request
  case class Problem(
    id: String,
    size: Int,
    operators: Set[String],
    solved: Option[Boolean],
    timeLeft: Option[Int])

  def myProblems(): Seq[Problem] = {
    implicit val resultFormatter = jsonFormat5(Problem)

    val result = Http(url(BaseUrl + "myproblems").POST <<? Params << "{}" OK as.String)

    result().asJson.convertTo[Seq[Problem]]
  }

  case class EvalRequest(
    id: Option[String],
    program: Option[String],
    arguments: Seq[String]) extends Request

  case class EvalResponse(
    status: String,
    outputs: Option[Seq[String]],
    message: Option[String])

  def eval(request: EvalRequest): EvalResponse = {
    implicit val requestFormatter = jsonFormat3(EvalRequest)
    implicit val resultFormatter = jsonFormat3(EvalResponse)

    val requestJson = request.toJson.compactPrint
    val result = Http(url(BaseUrl + "eval").POST <<? Params << requestJson OK as.String)

    result().asJson.convertTo[EvalResponse]
  }

  case class GuessRequest(
    id: String,
    program: String) extends Request

  case class GuessResponse(
    status: String,
    values: Option[Seq[String]],
    message: Option[String],
    lightning: Option[Boolean])

  def guess(request: GuessRequest): GuessResponse = {
    implicit val requestFormatter = jsonFormat2(GuessRequest)
    implicit val resultFormatter = jsonFormat4(GuessResponse)

    val requestJson = request.toJson.compactPrint
    val result = Http(url(BaseUrl + "guess").POST <<? Params << requestJson OK as.String)

    result().asJson.convertTo[GuessResponse]
  }


}
