import remote._

object Spider extends App {

  def opsList(size: Int) =
    if (size >= 11) List(Set.empty[String], Set("fold"), Set("tfold"))
    else if (size >= 8) List(Set.empty[String], Set("tfold"))
    else List(Set.empty[String])

  for {
    size ← 3 to 30
    ops ← opsList(size)
    count ← 1 to 5
  } {
    println(Remote.getTrainer(Some(size), Some(ops)))
    Thread.sleep(4500)
  }
}


object BonusSpider extends App {
  for {
    size ← List(42, 137)
    count ← 1 to 40
  } {
    println(Remote.getTrainer(Remote.TrainingRequest(Some(size), Some(Set()))))
    Thread.sleep(4500)
  }
}
