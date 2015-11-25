import scala.annotation.tailrec

object BowlingWithSemiColon {

  def compute_iter1(scores: Seq[(Int, Int)]): Int = scores.map(computeFrame).sum

  def computeFrame(frame: (Int, Int)): Int = frame._1 + frame._2

  @tailrec
  def compute(scores: Seq[(Int, Int)], acc: Int = 0): Int = scores match {
    case Nil => acc
    case (10, _) :: f1 :: f2 :: Nil => acc + 10 + computeFrame(f1) + computeFrame(f2) // Handle Last Strike
    case (10, _) :: f1 :: f2 :: t => compute(f1 :: f2 :: t, acc + 10 + computeFrame(f1) + computeFrame(f2)) // Handle Strike
    case (a, b) :: f :: Nil if a + b == 10 => acc + 10 + computeFrame(f) // Handle Last Spare
    case (a, b) :: f :: t if a + b == 10 => compute(f :: t, acc + 10 + computeFrame(f)) // Handle Spare
    case f :: t => compute(t, acc + computeFrame(f)) //Handle default
  }


}
