import scala._

object Pretty extends App {
  val exprs = List(
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Id("x"), Plus(Value(1), Shr1(Value(0))))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Id("x"), Plus(Value(1), Shr1(Value(0))))),
    Lambda1(Id("x"), Plus(If0(And(Id("x"), Value(1)), Id("x"), Value(1)), Shr1(Value(0)))),
    Lambda1(Id("x"), Plus(If0(And(Value(1), Id("x")), Id("x"), Value(1)), Shr1(Value(0)))),
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Id("x"), Plus(Value(1), Shr1(Value(1))))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Id("x"), Plus(Value(1), Shr1(Value(1))))),
    Lambda1(Id("x"), Plus(If0(And(Id("x"), Value(1)), Id("x"), Value(1)), Shr1(Value(1)))),
    Lambda1(Id("x"), Plus(If0(And(Value(1), Id("x")), Id("x"), Value(1)), Shr1(Value(1)))),
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Id("x"), Plus(Shr1(Value(0)), Value(1)))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Id("x"), Plus(Shr1(Value(0)), Value(1)))),
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Plus(Id("x"), Shr1(Value(0))), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Plus(Id("x"), Shr1(Value(0))), Value(1))),
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Id("x"), Shr1(Plus(Value(1), Value(1))))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Id("x"), Shr1(Plus(Value(1), Value(1))))),
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Id("x"), Plus(Shr1(Value(1)), Value(1)))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Id("x"), Plus(Shr1(Value(1)), Value(1)))),
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Plus(Id("x"), Shr1(Value(1))), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Plus(Id("x"), Shr1(Value(1))), Value(1))),
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Plus(Shr1(Value(0)), Id("x")), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Plus(Shr1(Value(0)), Id("x")), Value(1))),
    Lambda1(Id("x"), If0(And(Id("x"), Plus(Value(1), Shr1(Value(0)))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Plus(And(Id("x"), Value(1)), Shr1(Value(0))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Plus(Id("x"), Shr1(Value(0)))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Plus(And(Value(1), Id("x")), Shr1(Value(0))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Id("x"), Value(1)), Plus(Shr1(Value(1)), Id("x")), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Id("x")), Plus(Shr1(Value(1)), Id("x")), Value(1))),
    Lambda1(Id("x"), If0(And(Id("x"), Plus(Value(1), Shr1(Value(1)))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Plus(And(Id("x"), Value(1)), Shr1(Value(1))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Plus(Id("x"), Shr1(Value(1)))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Plus(And(Value(1), Id("x")), Shr1(Value(1))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Id("x"), Plus(Shr1(Value(0)), Value(1))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Plus(Id("x"), Shr1(Value(0))), Value(1)), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Id("x"), Shr1(Plus(Value(1), Value(1)))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Id("x"), Plus(Shr1(Value(1)), Value(1))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Plus(Id("x"), Shr1(Value(1))), Value(1)), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Shr1(Plus(And(Id("x"), Value(1)), Value(1))), Id("x"), Value(1))),
    Lambda1(Id("x"), Plus(Shr1(Value(0)), If0(And(Id("x"), Value(1)), Id("x"), Value(1)))),
    Lambda1(Id("x"), Plus(Shr1(Value(1)), If0(And(Id("x"), Value(1)), Id("x"), Value(1)))),
    Lambda1(Id("x"), If0(Plus(Shr1(Value(0)), And(Id("x"), Value(1))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Shr1(Plus(Value(1), And(Id("x"), Value(1)))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Plus(Shr1(Value(1)), And(Id("x"), Value(1))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Plus(Shr1(Value(0)), Id("x")), Value(1)), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Shr1(Plus(And(Value(1), Id("x")), Value(1))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Plus(Shr1(Value(1)), Id("x")), Value(1)), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Shr1(Plus(Id("x"), Id("x"))), Value(1)), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Plus(Shr1(Value(0)), Id("x"))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Plus(Value(1), Shr1(Value(0))), Id("x")), Id("x"), Value(1))),
    Lambda1(Id("x"), Plus(Shr1(Value(0)), If0(And(Value(1), Id("x")), Id("x"), Value(1)))),
    Lambda1(Id("x"), Plus(Shr1(Value(1)), If0(And(Value(1), Id("x")), Id("x"), Value(1)))),
    Lambda1(Id("x"), If0(Plus(Shr1(Value(0)), And(Value(1), Id("x"))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Shr1(Plus(Value(1), And(Value(1), Id("x")))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(Plus(Shr1(Value(1)), And(Value(1), Id("x"))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Plus(Shr1(Value(1)), Id("x"))), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Plus(Value(1), Shr1(Value(1))), Id("x")), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Plus(Shr1(Value(0)), Value(1)), Id("x")), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Shr1(Plus(Value(1), Value(1))), Id("x")), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Plus(Shr1(Value(1)), Value(1)), Id("x")), Id("x"), Value(1))),
    Lambda1(Id("x"), If0(And(Value(1), Shr1(Plus(Id("x"), Id("x")))), Id("x"), Value(1)))
  )


  def stringify(expr: Expr): String = expr match {

    case Value(value) ⇒ value.toString
    case Id(id) ⇒ id

    case Not(expr) ⇒ s"(not ${stringify(expr)})"
    case Shl1(expr) ⇒ s"(shl1 ${stringify(expr)})"
    case Shr1(expr) ⇒ s"(shr1 ${stringify(expr)})"
    case Shr4(expr) ⇒ s"(shr4 ${stringify(expr)})"
    case Shr16(expr) ⇒ s"(shr16 ${stringify(expr)})"

    case And(e1, e2) ⇒ s"(and ${stringify(e1)} ${stringify(e2)})"
    case Or(e1, e2) ⇒ s"(or ${stringify(e1)} ${stringify(e2)})"
    case Xor(e1, e2) ⇒ s"(xor ${stringify(e1)} ${stringify(e2)})"
    case Plus(e1, e2) ⇒ s"(plus ${stringify(e1)} ${stringify(e2)})"

    case If0(e1, e2, e3) ⇒ s"(if0 ${stringify(e1)} ${stringify(e2)} ${stringify(e3)})"
    case Fold(e1, e2, l) ⇒ s"(fold ${stringify(e1)} ${stringify(e2)} ${stringify(l)})"

    case Lambda1(id, body) ⇒ s"(lambda (${stringify(id)}) ${stringify(body)})"
    case Lambda2(id1, id2, body) ⇒ s"(lambda (${stringify(id1)} ${stringify(id2)}) ${stringify(body)})"

  }


  exprs map stringify foreach println
}
