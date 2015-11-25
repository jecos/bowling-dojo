import scala.annotation.tailrec

object Bowling {

  def compute_iter1(scores: Seq[(Int, Int)]): Int = scores.map(computeFrame).sum

  def computeFrame(frame: (Int, Int)): Int = frame._1 + frame._2

  @tailrec
  def compute(scores: Seq[(Int, Int)], acc: Int = 0): Int = scores match {
    case Nil => acc
    case s@Seq((10, _), f1, f2, t@_*) if t.isEmpty => acc + 10 + computeFrame(f1) + computeFrame(f2) // Handle Last Strike
    case s@Seq((10, _), f1, f2, _*) => compute(s.tail, acc + 10 + computeFrame(f1) + computeFrame(f2)) // Handle Strike
    case s@Seq((a, b), f, t@_*) if a + b == 10 && t.isEmpty => acc + 10 + computeFrame(f) // Handle Last Spare
    case s@Seq((a, b), f, _*) if a + b == 10 => compute(s.tail, acc + 10 + computeFrame(f)) // Handle Spare
    case s => compute(s.tail, acc + computeFrame(s.head)) //Handle default
  }
}
