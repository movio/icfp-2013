import util.parsing.combinator._

trait Expr {
  // meh. can't really return this.type; cf http://stackoverflow.com/questions/5331722/define-method-to-return-type-of-class-extending-it
  def fillWithRandomNames(names: List[String]): Expr = ???
}
case class Value(value: Long) extends Expr
case class Id(id: String) extends Expr

case class Not(e: Expr) extends Expr
case class Shl1(e: Expr) extends Expr {

  override def fillWithRandomNames(names: List[String]) = this match {
    case Shl1(PlaceHolder) ⇒ Shl1(Id(RandomUtils.pickRandom(names)))
    case _                 ⇒ Shl1(e.fillWithRandomNames(names))
  }

}
case class Shr1(e: Expr) extends Expr
case class Shr4(e: Expr) extends Expr
case class Shr16(e: Expr) extends Expr
case class And(l: Expr, r: Expr) extends Expr
case class Or(l: Expr, r: Expr) extends Expr
case class Xor(l: Expr, r: Expr) extends Expr
case class Plus(l: Expr, r: Expr) extends Expr

case class If0(p: Expr, t: Expr, f: Expr) extends Expr
case class Fold(x: Expr, init: Expr, lambda: Lambda2) extends Expr

case class Lambda1(id: Id, body: Expr) extends Expr {
  def argName = id.id
  override def fillWithRandomNames(names: List[String]) = this match {
    case Lambda1(_, PlaceHolder) ⇒ Lambda1(id, Id(RandomUtils.pickRandom(names)))
    case _                       ⇒ Lambda1(id, body.fillWithRandomNames(names))
  }
}
case class Lambda2(id1: Id, id2: Id, body: Expr) extends Expr {
  def argName1 = id1.id
  def argName2 = id2.id
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
}
