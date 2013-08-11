case object PlaceHolder extends Expr {
  def size = ???
}

object RandomUtils extends App {

  def pickRandom[T](choices: List[T]): T = choices(scala.util.Random.nextInt % choices.size)

  // http://rosettacode.org/wiki/Permutations_with_repetitions
  /**
   * Calculates all permutations taking n elements of the input List,
   * with repetitions.
   * Precondition: input.length > 0 && n > 0
   */
  def perms[T](input : Stream[T], n : Int) : Stream[List[T]] = {
    //require(input.length > 0 && n > 0)
    n match {
      case 1 ⇒ for (el ← input) yield List(el)
      case _ ⇒ for (el ← input; perm ← perms(input, n - 1)) yield el :: perm
    }
  }
  println(perms(Stream(1, 2, 3), 2).toList)
  println(perms(Stream(1), 2).toList)
  println(perms(Stream(1), 3).toList)
  println(perms(Stream(1, 2), 2).toList)
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

  type Program = Lambda1

  def fill(expr: Expr, ops: List[String], names: List[String]): Stream[Expr] = {
    def fillOps1(f: Expr ⇒ Expr) = ops.toStream.flatMap(op ⇒ fill(astForOp(op), ops diff Seq(op), names) map f)
    def fillNumbers1(f: Expr ⇒ Expr) = f(Value(0)) #:: f(Value(1)) #:: Stream.empty[Expr]
    def fillNames1(f: Expr ⇒ Expr) = names.toStream.map(n ⇒ f(Id(n)))
    def fill1(f: Expr ⇒ Expr) = fillNumbers1(f) ++ fillNames1(f) ++ fillOps1(f)

    expr match {
      case Not(PlaceHolder)             ⇒ fill1(Not(_))
      case Shl1(PlaceHolder)            ⇒ fill1(Shl1(_))
      case Shr1(PlaceHolder)            ⇒ fill1(Shr1(_))
      case Shr4(PlaceHolder)            ⇒ fill1(Shr4(_))
      case Shr16(PlaceHolder)           ⇒ fill1(Shr16(_))
      case Lambda1(arg, PlaceHolder)    ⇒ fill1(Lambda1(arg, _))
    }
  }

  val emptyProg = Lambda1(Id("a"), PlaceHolder)
  assert(fill(emptyProg, List(), List("a")) contains Lambda1(Id("a"), Value(0)))
  assert(fill(emptyProg, List(), List("a")) contains Lambda1(Id("a"), Value(1)))
  assert(fill(emptyProg, List(), List("a")) contains Lambda1(Id("a"), Id("a")))

  assert(fill(emptyProg, List("not"), List("a")) contains Lambda1(Id("a"), Id("a")))
  assert(fill(emptyProg, List("not"), List("a")) contains Lambda1(Id("a"), Not(Id("a"))))
  assert(fill(emptyProg, List("not"), List("a")) contains Lambda1(Id("a"), Not(Value(0))))

  assert(fill(emptyProg, List("not", "not"), List("a", "b")) contains Lambda1(Id("a"), Not(Not(Id("b")))))
  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Not(Not(Id("a")))))
  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Not(Not(Value(0)))))
  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Not(Not(Value(1)))))
  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Not(Value(1))))
  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Value(1)))
  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Value(0)))
  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Id("a")))

  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Id("a")))

  assert(fill(emptyProg, List("not", "not"), List("a")) contains Lambda1(Id("a"), Id("a")))
  assert(fill(emptyProg, List("shl1", "shr4"), List("a")) contains Lambda1(Id("a"), Shl1(Id("a"))))
  assert(fill(emptyProg, List("shl1", "shr4"), List("a")) contains Lambda1(Id("a"), Shr4(Id("a"))))
  assert(fill(emptyProg, List("shl1", "shr4"), List("a")) contains Lambda1(Id("a"), Shr4(Shl1(Id("a")))))
  assert(fill(emptyProg, List("shl1", "shr4"), List("a")) contains Lambda1(Id("a"), Shl1(Shr4(Id("a")))))

  //assert(fill(emptyProg, List("or"), List("a")) contains Lambda1(Id("a"), Or(Id("a"), Id("a"))))

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

  assert(fill(emptyProg, List("shl1"), List(emptyProg.argName)) contains Lambda1(Id("a"), Shl1(Id("a"))))

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

  def size(expr: Expr): Int = expr match {
    case Value(_) | Id(_) ⇒ 1
    case Not(e) ⇒ 1 + size(e)
    case Shl1(e) ⇒ 1 + size(e)
    case Shr1(e) ⇒ 1 + size(e)
    case Shr4(e) ⇒ 1 + size(e)
    case Shr16(e) ⇒ 1 + size(e)
    case And(e1, e2) ⇒ 1 + size(e1) + size(e2)
    case Or(e1, e2) ⇒ 1 + size(e1) + size(e2)
    case Xor(e1, e2) ⇒ 1 + size(e1) + size(e2)
    case Plus(e1, e2) ⇒ 1 + size(e1) + size(e2)
    case If0(e1, e2, e3) ⇒ 1 + size(e1) + size(e2) + size(e3)
    case Fold(e1, e2, l) ⇒ 1 + size(e1) + size(e2) + size(l)
    case Lambda1(_, body) ⇒ 1 + size(body)
    case Lambda2(_, _, body) ⇒ 1 + size(body)
  }

  val prog20 = "(lambda (x_38261) (fold x_38261 0 (lambda (x_38261 x_38262) (if0 x_38261 (or (shr4 (if0 (plus (shr4 (shl1 (shr4 x_38262))) x_38261) 1 x_38261)) x_38261) x_38261))))"
  assert(20 == size(Parser.parse(prog20).get))

  def size(ops: List[Expr]): Int = {
    ops.map {
      case _: Not ⇒ 1
      case _: Shl1 ⇒ 1
      case _: Shr1 ⇒ 1
      case _: Shr4 ⇒ 1
      case _: Shr16 ⇒ 1
      case _: And ⇒ 2
      case _: Or ⇒ 2
      case _: Xor ⇒ 2
      case _: Plus ⇒ 2
      case _: If0 ⇒ 3
      case _: Fold ⇒ 4
      case _ ⇒ ???
    }.sum + 2
  }

  assert(20 == size(List(
    Fold(PlaceHolder, PlaceHolder, Lambda2(Id(gensym), Id(gensym), PlaceHolder)),
    If0(PlaceHolder, PlaceHolder, PlaceHolder),
    Or(PlaceHolder, PlaceHolder),
    Shr4(PlaceHolder),
    If0(PlaceHolder, PlaceHolder, PlaceHolder),
    Plus(PlaceHolder, PlaceHolder),
    Shr4(PlaceHolder),
    Shl1(PlaceHolder),
    Shr4(PlaceHolder)
  )))

  // assert(4 == Set("lambda", "or") map opSize)
}
