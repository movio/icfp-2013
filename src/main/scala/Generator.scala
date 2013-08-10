case object PlaceHolder extends Expr {
  def size = ???
}

object RandomUtils {

  def pickRandom[T](choices: List[T]): T = choices(scala.util.Random.nextInt % choices.size)

  // http://stackoverflow.com/a/12525242
  def comb2[A](as: List[A]): List[(A, A)] = {
    require(as.size > 0, "need input.")
    if (as.size == 1) List((as(0), as(0)))
    else (List.fill(2)(as)).flatten.combinations(2).map(muh ⇒ (muh(0), muh(1))).toList
  }
}

case class ProgramTester(knownInputsToOutputs: Map[Long, Long]) { // extends Actor

  def testProgram(p: Generator.Program): Boolean = {
    knownInputsToOutputs.forall {
      case (input, expectedOutput) ⇒ Interpreter.evaluate(p, Map(p.argName → 1)) == expectedOutput
    }
  }
}

object Generator extends App {

  println(RandomUtils.comb2(List(1)))
  println(RandomUtils.comb2(List(1, 2)))
  println(RandomUtils.comb2(List(1, 2, 3)))
//  assert(List((1, 1)) == RandomUtils.combinations2(List(1, 2)))

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

    def fillOps2(f: (Expr, Expr) ⇒ Expr) =
      RandomUtils.comb2(ops).toStream.flatMap{ case (o1, o2) ⇒
        // WIP. not there yet.
        if (o1 != o2) {
          fill(astForOp(o1), ops diff List(o1, o2), names).zip(fill(astForOp(o2), ops diff List(o1, o2), names)).map {
            case (e1, e2) ⇒ f(e1, e2)
          }
        }
        else {
          fill(astForOp(o1), ops diff List(o1, o2), names).zip(fill(astForOp(o2), ops diff List(o1, o2), names)).map {
            case (e1, e2) ⇒ f(e1, e2)
          }
        }
      }

    def fillNumbers2(f: (Expr, Expr) ⇒ Expr) =
      f(Value(0), Value(0)) #:: f(Value(1), Value(0)) #:: f(Value(0), Value(1)) #:: f(Value(1), Value(1)) #:: Stream.empty[Expr]
    def fillNames2(f: (Expr, Expr) ⇒ Expr) =
      RandomUtils.comb2(names).toStream.flatMap { case (n1, n2) ⇒
        if (n1 != n2) { // order matters in this case
          f(Id(n1), Id(n2)) #:: f(Id(n1), Id(n2)) #:: Stream.empty[Expr]
        } else {
          f(Id(n1), Id(n2)) #:: Stream.empty[Expr]
        }
      }
    def fill2(f: (Expr, Expr) ⇒ Expr) = fillNumbers2(f) ++ fillNames2(f)


    expr match {
      case Not(PlaceHolder) ⇒ fill1(Not(_))
      case Shl1(PlaceHolder) ⇒ fill1(Shl1(_))
      case Shr1(PlaceHolder) ⇒ fill1(Shr1(_))
      case Shr4(PlaceHolder) ⇒ fill1(Shr4(_))
      case Shr16(PlaceHolder) ⇒ fill1(Shr16(_))
      case Or(PlaceHolder, PlaceHolder) ⇒ fill2(Or(_, _))
      case Lambda1(arg, PlaceHolder) ⇒ fill1(Lambda1(arg, _))
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

  assert(fill(emptyProg, List("or"), List("a")) contains Lambda1(Id("a"), Or(Id("a"), Id("a"))))

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

  // assert(4 == Set("lambda", "or") map opSize)

}
