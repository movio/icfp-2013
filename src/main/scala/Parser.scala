import util.parsing.combinator._

trait Expr {
  def size: Int
}

case class Value(value: Long) extends Expr {
  def size = 1
}

case class Id(id: String) extends Expr {
  def size = 1
}

// op1

case class Not(e: Expr) extends Expr {
  def size = 1 + e.size
}

case class Shl1(e: Expr) extends Expr {
  def size = 1 + e.size
}

case class Shr1(e: Expr) extends Expr {
  def size = 1 + e.size
}

case class Shr4(e: Expr) extends Expr {
  def size = 1 + e.size
}

case class Shr16(e: Expr) extends Expr {
  def size = 1 + e.size
}

// op2

case class And(l: Expr, r: Expr) extends Expr {
  def size = 1 + l.size + r.size
}

case class Or(l: Expr, r: Expr) extends Expr {
  def size = 1 + l.size + r.size
}

case class Xor(l: Expr, r: Expr) extends Expr {
  def size = 1 + l.size + r.size
}

case class Plus(l: Expr, r: Expr) extends Expr {
  def size = 1 + l.size + r.size
}

case class If0(p: Expr, t: Expr, f: Expr) extends Expr {
  def size = 1 + p.size + t.size + f.size
}

case class Fold(x: Expr, init: Expr, lambda: Lambda2) extends Expr {
  def size = 2 + x.size + init.size + lambda.size
}

case class Lambda1(id: Id, body: Expr) extends Expr {
  def argName = id.id
  def size = 1 + body.size
}
case class Lambda2(id1: Id, id2: Id, body: Expr) extends Expr {
  def argName1 = id1.id
  def argName2 = id2.id
  def size = body.size
}

object Parser extends RegexParsers {

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
      case "not"   ⇒ Not(e)
      case "shl1"  ⇒ Shl1(e)
      case "shr1"  ⇒ Shr1(e)
      case "shr4"  ⇒ Shr4(e)
      case "shr16" ⇒ Shr16(e)
    }
  }

  def op2 = ("(" ~> ("and" | "or" | "xor" | "plus") ~ expr ~ expr <~ ")") ^^ {
    case opName ~ e1 ~ e2 ⇒ opName match {
      case "and"  ⇒ And(e1, e2)
      case "or"   ⇒ Or(e1, e2)
      case "xor"  ⇒ Xor(e1, e2)
      case "plus" ⇒ Plus(e1, e2)
    }
  }

  def P = lambda1

  def parse(s: String) = parseAll(P, s)

  def getSize(prog: String): Int = {
    val progAST = parseAll(P, prog).get
    progAST.size
  }
}

object ASTTests extends App {
  // expr size
  assert(1 == Parser.parseAll(Parser.expr, "1").get.size)
  assert(1 == Parser.parseAll(Parser.expr, "0").get.size)
  assert(1 == Parser.parseAll(Parser.expr, "x").get.size)
  assert(2 == Parser.parseAll(Parser.expr, "(not 1)").get.size)
  assert(2 == Parser.parseAll(Parser.expr, "(shl1 1)").get.size)
  assert(2 == Parser.parseAll(Parser.expr, "(shr1 1)").get.size)
  assert(2 == Parser.parseAll(Parser.expr, "(shr4 1)").get.size)
  assert(2 == Parser.parseAll(Parser.expr, "(shr16 1)").get.size)
  assert(3 == Parser.parseAll(Parser.expr, "(and 1 1)").get.size)
  assert(3 == Parser.parseAll(Parser.expr, "(or 1 1)").get.size)
  assert(3 == Parser.parseAll(Parser.expr, "(xor 1 1)").get.size)
  assert(3 == Parser.parseAll(Parser.expr, "(plus 1 1)").get.size)
  assert(4 == Parser.parseAll(Parser.expr, "(if0 0 0 1)").get.size)
  assert(5 == Parser.parseAll(Parser.expr, "(fold 0 0 (lambda (x y) 0))").get.size)

  // program size
  assert(3 == Parser.getSize("(lambda (x) (not x))"))
  assert(3 == Parser.getSize("(lambda (x) (not 0))"))
  assert(3 == Parser.getSize("(lambda (x) (shr1 0))"))
  assert(4 == Parser.getSize("(lambda (x) (plus x 0))"))
  assert(8 == Parser.getSize("(lambda (x) (fold x 0 (lambda (y z) (or y z))))"))
  assert(16 == Parser.getSize("(lambda (x_37132) (fold (if0 (shr4 (plus (shr1 1) x_37132)) 0 x_37132) x_37132 (lambda (x_37133 x_37134) (if0 x_37134 x_37133 x_37133))))"))
}
