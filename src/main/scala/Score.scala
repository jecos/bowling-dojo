import scala.annotation.tailrec

object Score {

  @tailrec
  def compute(scores: Seq[(Int)], acc: Int = 0): Int = scores match {
    case Seq(10, s1, s2, t@_*) if t.isEmpty => acc + 10 + s1 + s2 // Handle Bonus Strike
    case s@Seq(10, s1, s2, _*) => compute(s.tail, acc + 10 + s1 + s2) // Handle Strike
    case Seq(a, b, c, t@_*) if a + b == 10 && t.isEmpty => acc + 10 + c // Handle Bonus Spare
    case s@Seq(a, b, c, _*) if a + b == 10 => compute(s.drop(2), acc + 10 + c) // Handle Spare
    case s@Seq(a, b, _*) => compute(s.drop(2), acc + a + b) //Handle default
    case Nil => acc
  }
}
