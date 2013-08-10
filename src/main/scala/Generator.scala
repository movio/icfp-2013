case object PlaceHolder extends Expr

object RandomUtils {

  def pickRandom[T](choices: List[T]): T = choices(scala.util.Random.nextInt % choices.size)

}

case class ProgramTester(knownInputsToOutputs: Map[Long, Long]) { // extends Actor

  def testProgram(p: Generator.Program): Boolean = {
    knownInputsToOutputs.forall {
      case (input, expectedOutput) ⇒ Interpreter.evaluate(p, Map(p.argName → 1)) == expectedOutput
    }
  }
}

object Generator extends App {

  private var counter = 0
  def gensym = {
    counter += 1
    s"x$counter"
  }

  def astForOp(op: String) = op match {
    case "not"   ⇒ Not(PlaceHolder)
    case "shl1"  ⇒ Shl1(PlaceHolder)
    case "shr1"  ⇒ Shr1(PlaceHolder)
    case "shr4"  ⇒ Shr4(PlaceHolder)
    case "shr16" ⇒ Shr16(PlaceHolder)
    case "and"   ⇒ And(PlaceHolder, PlaceHolder)
    case "or"    ⇒ Or(PlaceHolder, PlaceHolder)
    case "xor"   ⇒ Xor(PlaceHolder, PlaceHolder)
    case "plus"  ⇒ Plus(PlaceHolder, PlaceHolder)
    case "if0"   ⇒ If0(PlaceHolder, PlaceHolder, PlaceHolder)
    case "fold"  ⇒ Fold(PlaceHolder, PlaceHolder, Lambda2(Id(gensym), Id(gensym), PlaceHolder))
    case "tfold" ⇒
      val inputName = gensym
      Lambda1(
        Id(inputName),
        Fold(Id(inputName), Value(0), Lambda2(Id(gensym), Id(gensym), PlaceHolder))
      )
  }

  // ops ::= op1 | op2 | "if0" | "tfold" | "fold"
  assert(astForOp("if0").isInstanceOf[If0])
  assert(gensym.startsWith("x"))
  assert(gensym != gensym)
  assert(astForOp("tfold").isInstanceOf[Lambda1])
  assert(astForOp("fold").isInstanceOf[Fold])
  // op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
  assert(astForOp("not").isInstanceOf[Not])
  assert(astForOp("shl1").isInstanceOf[Shl1])
  assert(astForOp("shr1").isInstanceOf[Shr1])
  assert(astForOp("shr4").isInstanceOf[Shr4])
  assert(astForOp("shr16").isInstanceOf[Shr16])
  // op2 ::= "and" | "or" | "xor" | "plus"
  assert(astForOp("and").isInstanceOf[And])
  assert(astForOp("or").isInstanceOf[Or])
  assert(astForOp("xor").isInstanceOf[Xor])
  assert(astForOp("plus").isInstanceOf[Plus])

  //    {
  //        "id": "0Q0hlUyfQA4kvJa6YFpA7VSn",
  //        "size": 3,
  //        "operators": [
  //            "shl1"
  //        ],
  //        "solved": true,
  //        "timeLeft": 0
  //    },

  // solution:

  // {"id":"0Q0hlUyfQA4kvJa6YFpA7VSn",
  //    "program":"(lambda (x) (shl1 x))"}

  type Program = Lambda1

  def fillAST[T <: Expr](expr: T, opsLeft: List[String], names: List[String]): T =
    if (opsLeft.isEmpty) expr.fillWithRandomNames(names).asInstanceOf[T]
    else expr match {
      case Not(PlaceHolder) ⇒ ???
      case Shl1(PlaceHolder) ⇒ ???
      case Shr1(PlaceHolder) ⇒ ???
      case Shr4(PlaceHolder) ⇒ ???
      case Shr16(PlaceHolder) ⇒ ???
      case And(PlaceHolder, PlaceHolder) ⇒ ???
      case Or(PlaceHolder, PlaceHolder) ⇒ ???
      case Xor(PlaceHolder, PlaceHolder) ⇒ ???
      case Plus(PlaceHolder, PlaceHolder) ⇒ ???
      case If0(PlaceHolder, PlaceHolder, PlaceHolder) ⇒ ???
      case Fold(PlaceHolder, PlaceHolder, Lambda2(Id(arg1), Id(arg2), PlaceHolder)) ⇒ ???
      case Lambda1(arg, PlaceHolder) ⇒
        //fillAST(expr.copy(body = astForOp(opsLeft.head)), opsLeft.tail, names)
        // meh. not sure how to trick the type at the moment.
        fillAST(new Lambda1(arg, astForOp(opsLeft.head)), opsLeft.tail, names).asInstanceOf[T]
    }

  def generatePrograms(ops: Set[String], size: Int): Stream[Program] = {
    def programSkeleton(inputName: String) = Lambda1(Id(inputName), PlaceHolder)

    if (ops contains "tfold") ???
    else {
      if (ops.size + 2 == size) {
        val inputName = gensym
        Stream(fillAST(programSkeleton(inputName), ops.toList, List(inputName)))

      } else if (ops.size + 3 == size) {
        val inputName = gensym
        Stream(fillAST(programSkeleton(inputName), ops.toList, List(inputName)))

      } else ???
    }

  }

  def fill(expr: Expr, ops: Set[String], names: List[String]): Stream[Expr] =
    expr match {
      case Lambda1(arg, PlaceHolder) ⇒
        Lambda1(arg, Value(0)) #:: Lambda1(arg, Value(1)) #:: names.map(n ⇒ Lambda1(arg, Id(n))).toStream
    }

  val emptyProg = Lambda1(Id("a"), PlaceHolder)
  assert(fill(emptyProg, Set(), List("a")) contains Lambda1(Id("a"), Value(0)))
  assert(fill(emptyProg, Set(), List("a")) contains Lambda1(Id("a"), Value(1)))
  assert(fill(emptyProg, Set(), List("a")) contains Lambda1(Id("a"), Id("a")))

  // too hard.
  // always
  // 1 inputName / lambda
  // given operations fill size
  // not 1 + 1
  // and 1 + 2
  // or  1 + 2

  // def opSize(op: String): Int = op match {
  //   case "lambda" | "not"              ⇒ 1
  //   case "or" | "xor" | "and" | "plus" ⇒ 2
  // }

  // assert(4 == Set("lambda", "or") map opSize)

  // op1
  List(("not", -2L), ("shl1", 2L), ("shr1", 0L), ("shr4", 0L), ("shr16", 0L)) foreach {
    case (op, expected) ⇒
      assert(1 == generatePrograms(Set(op), size = 3).size)
      val genProgram = generatePrograms(Set(op), size = 3).head
      println(genProgram)
      assert(expected == Interpreter.evaluate(genProgram, Map(genProgram.argName → 1)))
  }

  // op2
  List(("and", 1L), ("or", 1L), ("xor", 0L), ("plus", 2L)) foreach {
    case (op, expected) ⇒
      assert(1 == generatePrograms(Set(op), size = 4).size)
      val genProgram = generatePrograms(Set(op), size = 4).head
      println(genProgram)
      assert(ProgramTester(Map(1L → expected)).testProgram(genProgram))
      assert(expected == Interpreter.evaluate(genProgram, Map(genProgram.argName → 1)))
  }

}
