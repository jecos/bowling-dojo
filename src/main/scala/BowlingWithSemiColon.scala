import scala.annotation.tailrec

object BowlingWithSemiColon {


  @tailrec
  def compute(scores: Seq[Int], acc: Int = 0): Int = scores match {
    case 10 :: s1 :: s2 :: Nil => acc + 10 + s1 + s2 // Handle Bonus Strike
    case 10 :: s1 :: s2 :: t => compute(s1 :: s2 :: t, acc + 10 + s1 + s2) // Handle Strike
    case a :: b :: c :: Nil if a + b == 10 => acc + 10 + c // Handle Bonus Spare
    case a :: b :: c :: t if a + b == 10 => compute(c :: t, acc + 10 + c) // Handle Spare
    case a :: b :: t => compute(t, acc + a + b) //Handle default
    case Nil => acc
  }


}
