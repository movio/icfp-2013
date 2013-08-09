import util.parsing.combinator._

trait Expr
case class Value(value: Long) extends Expr
case class Id(id: String) extends Expr

trait Op extends Expr
trait Op1 extends Op
case class Not(e: Expr) extends Op1
case class Shl1(e: Expr) extends Op1
case class Shr1(e: Expr) extends Op1
case class Shr4(e: Expr) extends Op1
case class Shr16(e: Expr) extends Op1
trait Op2 extends Op
case class And(l: Expr, r: Expr) extends Op2
case class Or(l: Expr, r: Expr) extends Op2
case class Xor(l: Expr, r: Expr) extends Op2
case class Plus(l: Expr, r: Expr) extends Op2

case class If0(p: Expr, t: Expr, f: Expr) extends Expr
case class Fold(x: Expr, init: Expr, lambda: Lambda2) extends Expr

trait Lambda extends Expr
case class Lambda1(id: Id, body: Expr) extends Lambda
case class Lambda2(id1: Id, id2: Id, body: Expr) extends Lambda

trait Parser extends RegexParsers {

  def expr: Parser[Expr] = zero | one | id | if0 | fold | op2 | op1

  def zero = "0" ^^^ Value(0)
  def one = "1" ^^^ Value(1)
  def id = """[a-z][a-z_0-9]*""".r ^^ Id

  def if0 = ("(" ~ "if0" ~> expr ~ expr ~ expr <~ ")") ^^ {
    case e1 ~ e2 ~ e3 ⇒ If0(e1, e2, e3)
  }

  def lambda1 = ("(" ~ "lambda" ~ "(" ~> id ~ ")" ~ expr <~ ")") ^^ {
    case id ~ _ ~ expr ⇒ Lambda1(id, expr)
  }

  def lambda2 = ("(" ~ "lambda" ~ "(" ~> id ~ id ~ ")" ~ expr <~ ")") ^^ {
    case id1 ~ id2 ~ _ ~ expr ⇒ Lambda2(id1, id2, expr)
  }

  def fold = ("(" ~ "fold" ~> expr ~ expr ~ lambda2 <~ ")") ^^ {
    case e ~ init ~ lambda ⇒ Fold(e, init, lambda)
  }

  def op1 = ("(" ~> ("not" | "shl1" | "shr16" | "shr1" | "shr4") ~ expr <~ ")") ^^ {
    case opName ~ e ⇒ opName match {
      case "not" ⇒ Not(e)
      case "shl1" ⇒ Shl1(e)
      case "shr1" ⇒ Shr1(e)
      case "shr4" ⇒ Shr4(e)
      case "shr16" ⇒ Shr16(e)
    }
  }

  def op2 = ("(" ~> ("and" | "or" | "xor" | "plus") ~ expr ~ expr <~ ")") ^^ {
    case opName ~ e1 ~ e2 ⇒ opName match {
      case "and" ⇒ And(e1, e2)
      case "or" ⇒ Or(e1, e2)
      case "xor" ⇒ Xor(e1, e2)
      case "plus" ⇒ Plus(e1, e2)
    }
  }

  def P = lambda1

  def parse(s: String) = parseAll(P, s)
}

object Program extends Parser with App {

  def evaluate(expr: Expr, env: Map[String, Long]): Long = expr match {
    case Value(v) ⇒ v
    case Id(id) ⇒ env(id)

    // op1
    case Not(e) ⇒ ~(evaluate(e, env))
    case Shl1(e) ⇒ (evaluate(e, env))  << 1
    case Shr1(e) ⇒ (evaluate(e, env))  >> 1
    case Shr4(e) ⇒ (evaluate(e, env))  >> 4
    case Shr16(e) ⇒ (evaluate(e, env)) >> 16

    // op2
    case And(l, r) ⇒ evaluate(l, env)  & evaluate(r, env)
    case Or(l, r) ⇒ evaluate(l, env)   | evaluate(r, env)
    case Xor(l, r) ⇒ evaluate(l, env)  ^ evaluate(r, env)
    case Plus(l, r) ⇒ evaluate(l, env) + evaluate(r, env)

    case If0(p, t, f) ⇒
      if (evaluate(p, env) == 0L) evaluate(t, env)
      else evaluate(f, env)

    case Fold(e, i, l) ⇒
      val (num, init)  = (evaluate(e, env), evaluate(i, env))
      (0 to 7).foldLeft(init) { (memo: Long, shiftFactor: Int) ⇒
        val numPart: Long = (num >> shiftFactor * 8) & 0xFF
        evaluate(l, env ++ Map(l.id1.id → numPart, l.id2.id → memo))
      }

    // lambda
    case Lambda1(_, b) ⇒ (evaluate(b, env))
    case Lambda2(_, _, b) ⇒ (evaluate(b, env))
  }

  assert(1     == print(evaluate(parseAll(expr, "x").get, Map("x" →    1L))))
  assert(1     == print(evaluate(parseAll(expr, "1").get, Map())))
  assert(-2    == print(evaluate(parseAll(expr, "(not 1)").get, Map())))
  assert(2     == print(evaluate(parseAll(expr, "(shl1 1)").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(shr1 1)").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(shr16 1)").get, Map())))
  assert(64    == print(evaluate(parseAll(P, "(lambda (x) x)").get, Map("x" → 64L))))
  assert(2     == print(evaluate(parseAll(P, "(lambda (x) (shr16 x))").get, Map("x" → Math.pow(2, 17).toLong))))
  assert(1     == print(evaluate(parseAll(expr, "(and 1 1)").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(and 1 0)").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(and 1 (shr16 1))").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(and 0 1)").get, Map())))
  assert(1     == print(evaluate(parseAll(expr, "(or 0 1)").get, Map())))
  assert(1     == print(evaluate(parseAll(expr, "(or (shr4 1) 1)").get, Map())))
  assert(1     == print(evaluate(parseAll(expr, "(xor (shr4 1) 1)").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(xor 1 1)").get, Map())))
  assert(2     == print(evaluate(parseAll(expr, "(plus 1 1)").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(if0 1 1 0)").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(if0 0 0 1)").get, Map())))
  assert(0     == print(evaluate(parseAll(expr, "(fold 0 0 (lambda (x y) (and x y)))").get, Map())))
  assert(0xFF  == print(evaluate(parseAll(P, "(lambda (x) (fold x 0 (lambda (y z) (or y z))))").get, Map("x" → 0x1122334455667788L))))
  assert(36    == print(evaluate(parseAll(P, "(lambda (x) (fold x 0 (lambda (y z) (plus y z))))").get, Map("x" →  0x0102030405060708L))))
  assert(0xFF  == print(evaluate(parseAll(P, "(lambda (x) (fold x 0 (lambda (y z) (if0 y y y))))").get, Map("x" → 0xFF00000000000000L))))
  assert(0     == print(evaluate(parseAll(P, "(lambda (x) (fold x 0 (lambda (y z) (if0 y y y))))").get, Map("x" → 0x00000000000000FFL))))

  def print(long: Long) = {
    println(long.toBinaryString)
    long
  }

}
