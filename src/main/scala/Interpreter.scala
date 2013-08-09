import Parser._

object Interpreter {

  def eval(prog: String, inputs: Long*): Seq[Long] = {
    println
    print("prog: ")
    println(prog)

    val outputs = inputs map { input ⇒
      println
      print("in:   ")
      output(input)

      val result = evaluate(parseAll(P, prog).get, Map("x" -> input))
      print("out:  ")
      output(result)
    }

    println
    outputs
  }

  def output(long: Long) = {
    val str = long.toBinaryString
    for (_ ← 1 to (64 - str.length)) { print("0") }
    println(long.toBinaryString)
    long
  }

  def evaluate(expr: Expr, env: Map[String, Long]): Long = expr match {
    case Value(v)   ⇒ v
    case Id(id)     ⇒ env(id)

    // op1
    case Not(e)     ⇒ ~(evaluate(e, env))
    case Shl1(e)    ⇒ (evaluate(e, env)) << 1
    case Shr1(e)    ⇒ (evaluate(e, env)) >> 1
    case Shr4(e)    ⇒ (evaluate(e, env)) >> 4
    case Shr16(e)   ⇒ (evaluate(e, env)) >> 16

    // op2
    case And(l, r)  ⇒ evaluate(l, env) & evaluate(r, env)
    case Or(l, r)   ⇒ evaluate(l, env) | evaluate(r, env)
    case Xor(l, r)  ⇒ evaluate(l, env) ^ evaluate(r, env)
    case Plus(l, r) ⇒ evaluate(l, env) + evaluate(r, env)

    case If0(p, t, f) ⇒
      if (evaluate(p, env) == 0L) evaluate(t, env)
      else evaluate(f, env)

    case Fold(e, i, l) ⇒
      val (num, init) = (evaluate(e, env), evaluate(i, env))
      (0 to 7).foldLeft(init) { (memo: Long, shiftFactor: Int) ⇒
        val numPart: Long = (num >> shiftFactor * 8) & 0xFF
        evaluate(l, env ++ Map(l.id1.id → numPart, l.id2.id → memo))
      }

    // lambda
    case Lambda1(_, b)    ⇒ (evaluate(b, env))
    case Lambda2(_, _, b) ⇒ (evaluate(b, env))
  }

  assert(1 == evaluate(parseAll(expr, "x").get, Map("x" → 1L)))
  assert(1 == evaluate(parseAll(expr, "1").get, Map()))
  assert(-2 == evaluate(parseAll(expr, "(not 1)").get, Map()))
  assert(2 == evaluate(parseAll(expr, "(shl1 1)").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(shr1 1)").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(shr16 1)").get, Map()))
  assert(64 == evaluate(parseAll(P, "(lambda (x) x)").get, Map("x" → 64L)))
  assert(2 == evaluate(parseAll(P, "(lambda (x) (shr16 x))").get, Map("x" → Math.pow(2, 17).toLong)))
  assert(1 == evaluate(parseAll(expr, "(and 1 1)").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(and 1 0)").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(and 1 (shr16 1))").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(and 0 1)").get, Map()))
  assert(1 == evaluate(parseAll(expr, "(or 0 1)").get, Map()))
  assert(1 == evaluate(parseAll(expr, "(or (shr4 1) 1)").get, Map()))
  assert(1 == evaluate(parseAll(expr, "(xor (shr4 1) 1)").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(xor 1 1)").get, Map()))
  assert(2 == evaluate(parseAll(expr, "(plus 1 1)").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(if0 1 1 0)").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(if0 0 0 1)").get, Map()))
  assert(0 == evaluate(parseAll(expr, "(fold 0 0 (lambda (x y) (and x y)))").get, Map()))
  assert(0xFF == evaluate(parseAll(P, "(lambda (x) (fold x 0 (lambda (y z) (or y z))))").get, Map("x" → 0x1122334455667788L)))
  assert(36 == evaluate(parseAll(P, "(lambda (x) (fold x 0 (lambda (y z) (plus y z))))").get, Map("x" → 0x0102030405060708L)))
  assert(0xFF == evaluate(parseAll(P, "(lambda (x) (fold x 0 (lambda (y z) (if0 y y y))))").get, Map("x" → 0xFF00000000000000L)))
  assert(0 == evaluate(parseAll(P, "(lambda (x) (fold x 0 (lambda (y z) (if0 y y y))))").get, Map("x" → 0x00000000000000FFL)))
}
