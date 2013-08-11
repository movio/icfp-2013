
import remote.Remote

object TeamKiwi extends App {

  val notDone = Remote.myProblems.filter(p ⇒ {
    // exclude done
    if (p.timeLeft.isDefined)
      if (p.timeLeft.get > 0)
        true
      else
        false
    else
      true
  })

  val noFolds = notDone.filterNot(p ⇒ p.operators.contains("fold") || p.operators.contains("tfold"))

  val smallestFirst = noFolds.sortWith((x, y) ⇒ x.size < y.size)

  smallestFirst foreach println

}