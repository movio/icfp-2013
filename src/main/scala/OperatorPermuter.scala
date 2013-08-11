import Interpreter._

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
        val (partial, ops) = next
        if (partial.head == PlaceHolder)
          acc ++ perms(Value(0) :: partial.tail, ops) ++ perms(Value(1) :: partial.tail, ops) ++ perms(Id("x") :: partial.tail, ops)
        else
          acc ++ perms(partial, ops)
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

  println("starting")
  val start = System.nanoTime

  val stuff = perms(Nil,
    List(
      If0(PlaceHolder, PlaceHolder, PlaceHolder),
      And(PlaceHolder, PlaceHolder),
      Plus(PlaceHolder, PlaceHolder),
      Shr1(PlaceHolder),
      PlaceHolder,
      PlaceHolder,
      PlaceHolder,
      PlaceHolder,
      PlaceHolder
    )
  )

//  val stuff = perms(Nil,
//    List(
//      Fold(PlaceHolder, PlaceHolder, Lambda2(Id("y"), Id("z"), PlaceHolder)),
//      Xor(PlaceHolder, PlaceHolder),
//      Shl1(PlaceHolder),
//      Shr16(PlaceHolder),
//      Shr16(PlaceHolder),
//      Shr4(PlaceHolder),
//      Shr4(PlaceHolder),
//      PlaceHolder,
//      PlaceHolder,
//      PlaceHolder,
//      PlaceHolder
//    )
//  )

  //stuff foreach println
  val real = stuff map { list ⇒ Lambda1(Id("x"), list.head) }
  val duration = System.nanoTime - start
  println(f"duration: ${duration / 1000000.0}%.02fms")


  println("running test program")

  val inputs = Seq(0x342abcL, 0x23498deb29L, 0xaab234L, 0x2983712298371L, 0xbdaefcabdefcL)
  val outputs = Seq(0x342abcL, 0x1L, 0xaab234L, 0x1L, 0xbdaefcabdefcL)

  val matching = real filter { expr ⇒ eval(expr, inputs: _*) == outputs } //map { expr ⇒ eval(expr, inputs: _*) } foreach println //filter { _ == outputs }
  //println(outputs)
  matching foreach println
  println(matching.size)

  println(stuff.size)
}
