case object PlaceHolder extends Expr

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

}
