// package remote

// import akka.actor.Actor
// import akka.actor.ActorLogging
// import scala.concurrent._
// import scala.util.Success
// import akka.actor.ActorRef
// import scala.util.Failure
// import akka.pattern._
// import scala.concurrent.duration._
// import Remote.EvalRequest
// import Remote.EvalResponse
// import Remote.Problem
// import akka.actor.actorRef2Scala
// import akka.util.Timeout.durationToTimeout

// object Worker {
//   case object Gimme
//   case class NewProblem(problem: Remote.Problem)
//   case class ProblemComplete(id: String)
//   case class Solution(id: String, program: String)
//   case class SolutionResponse(id: String, guessResponse: Remote.GuessResponse)

//   case class Op() // Need to put this back in Parse
// }

// abstract class Worker(coordinator: ActorRef, sender: ActorRef) extends Actor with ActorLogging {
//   import Worker._
//   import Remote._
//   implicit val exec = context.dispatcher

//   def createSolution(operators: Seq[Op], size: Int, values: Seq[(Long, Long)]): Solution

//   def receive = {
//     case NewProblem(p) ⇒
//       {
//         // FIXME - map to hex
//         val startingArgs = List(0, 1, 10, 15, 21, 27, 32, 1001).map(_.toString)
//         val eval = ((sender ? EvalRequest(Some(p.id), None, startingArgs))(30 seconds)).asInstanceOf[Future[EvalResponse]]

//         eval.onComplete {
//           case Success(response) ⇒ {
//             val results: Seq[(Long, Long)] = null //FIXME response.outputs
//             haveAGo(p, results)
//           }
//           case Failure(t) ⇒ //FIXME
//         }
//       }

//       def haveAGo(problem: Problem, results: Seq[(Long, Long)]) {
//         val operators = problem.operators.map(_.asInstanceOf[Op]) // FIXME this wont work
//         val solution = future { createSolution(operators, problem.size, results) }

//         def guess(solution: Solution): Future[SolutionResponse] = {
//           (sender ? Remote.GuessRequest(solution.id, solution.program))(30 seconds).asInstanceOf[Future[SolutionResponse]]
//         }

//         val result = for {
//           s ← solution
//           response ← guess(s)
//         } yield response

//         result.onComplete {
//           case Success(response) ⇒ {
//             response.guessResponse.status match {
//               case "win" ⇒ coordinator ! ProblemComplete(response.id)
//               case "mismatch" ⇒ {
//                 val newResults: Seq[(Long, Long)] = null //FIXME response.guessResponse.values
//                 haveAGo(problem, newResults ++ results)
//               }
//               case "error" ⇒ // FIXME
//             }
//             coordinator ! ProblemComplete(response.id)
//           }
//           case Failure(t) ⇒ // FIXME
//         }
//       }
//   }

// }
