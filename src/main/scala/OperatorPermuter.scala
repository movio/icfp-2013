import scala._

object OperatorPermuter extends App {

  val ops = List(Shl1(PlaceHolder), Not(PlaceHolder), Shr4(PlaceHolder))

  def perms(partial: List[Expr], ops: List[Expr]): List[List[Expr]] = {

    if (ops.isEmpty) List(partial)
    else {
      val validOps = ops.distinct.filter { op ⇒ size(op) <= partial.size }

      val nextIter = validOps map {
        case PlaceHolder ⇒ (PlaceHolder :: partial, ops diff List(PlaceHolder))
        case e: Not      ⇒ (Not(partial.head) :: partial.tail, ops diff List(e))
        case e: Shl1     ⇒ (Shl1(partial.head) :: partial.tail, ops diff List(e))
        case e: Shr1     ⇒ (Shr1(partial.head) :: partial.tail, ops diff List(e))
        case e: Shr4     ⇒ (Shr4(partial.head) :: partial.tail, ops diff List(e))
        case e: Shr16    ⇒ (Shr16(partial.head) :: partial.tail, ops diff List(e))

        case e: And ⇒ partial match {
          case e1 :: e2 :: rest ⇒ (And(e1, e2) :: rest, ops diff List(e))
        }
        case e: Or ⇒ partial match {
          case e1 :: e2 :: rest ⇒ (Or(e1, e2) :: rest, ops diff List(e))
        }
        case e: Xor ⇒ partial match {
          case e1 :: e2 :: rest ⇒ (Xor(e1, e2) :: rest, ops diff List(e))
        }
        case e: Plus ⇒ partial match {
          case e1 :: e2 :: rest ⇒ (Plus(e1, e2) :: rest, ops diff List(e))
        }

        case e: If0 ⇒ partial match {
          case e1 :: e2 :: e3 :: rest ⇒ (If0(e1, e2, e3) :: rest, ops diff List(e))
        }

        case e: Fold ⇒ partial match {
          case e1 :: e2 :: e3 :: rest ⇒ (Fold(e1, e2, Lambda2(Id("y"), Id("z"), e3)) :: rest, ops diff List(e))
        }
      }

      nextIter.foldLeft(List.empty[List[Expr]]) { (acc, next) ⇒
        acc ++ perms(next._1, next._2)
      }
    }
  }

  def size(e: Expr) = e match {
    case PlaceHolder ⇒ 0
    case _: Not      ⇒ 1
    case _: Shl1     ⇒ 1
    case _: Shr1     ⇒ 1
    case _: Shr4     ⇒ 1
    case _: Shr16    ⇒ 1
    case _: And      ⇒ 2
    case _: Or       ⇒ 2
    case _: Xor      ⇒ 2
    case _: Plus     ⇒ 2
    case _: If0      ⇒ 3
    case _: Fold     ⇒ 3 // 3 expressions, so 3, not 4
  }

  val stuff = perms(List(PlaceHolder),
    List(
      If0(PlaceHolder, PlaceHolder, PlaceHolder),
      And(PlaceHolder, PlaceHolder),
      And(PlaceHolder, PlaceHolder),
      Shr1(PlaceHolder),
      PlaceHolder,
      PlaceHolder,
      PlaceHolder,
      PlaceHolder
    )
  )

  stuff foreach println
  println(stuff.size)
}
