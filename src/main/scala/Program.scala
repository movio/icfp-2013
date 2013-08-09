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
case class Fold(x: Expr, init: Expr, lambda: Lambda) extends Expr

trait Lambda extends Expr
case class Lambda1(id: Id, body: Expr) extends Lambda
case class Lambda2(id1: Id, id2: Id, body: Expr) extends Lambda

trait Parser extends RegexParsers {

  def expr: Parser[Expr] = zero | one | id | if0 | fold | op1 | op2

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

  def op1 = ("(" ~> ("not" | "shl1" | "shr1" | "shr4" | "shr16") ~ expr <~ ")") ^^ {
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

  def e: Parser[_] =
    "0" | "1" | id |
      ("(" ~ "if0" ~ e ~ e ~ e ~ ")") |
      ("(" ~ "fold" ~ e ~ e ~ "(" ~ "lambda" ~ "(" ~ id ~ id ~ ")" ~ e ~ ")" ~ ")") |
      ("(" ~ op1 ~ e ~ ")") |
      ("(" ~ op2 ~ e ~ e ~ ")")

  def P = lambda1

  def parse(s: String) = parseAll(P, s)
}

object Program extends Parser with App {

  def evaluate(p: Lambda1, input: Long) {

  //   (ourprogram 1)
  //   (fold x
  // replace id's from outside to inside, or vice versa?
  // ((lambda (x) (fold x 0 (lambda (y z) (or y z)))) 1)
  // (fold x 0 (...)) { x ⇒ 1 }

  }

  val p = "(lambda (x) (fold x 0 (lambda (y z) (or y z))))"
  println(parse(p))

  //evaluate(parse(???), 1L)l

  //println(parseAll(if0, "(if0 1 1 1)"))
  //println(parseAll(lambda1, "(lambda (x) 1)"))
  //println(parseAll(lambda2, "(lambda (x y) 1)"))
  //println(parseAll(op2, "(and 1 0)"))
}
