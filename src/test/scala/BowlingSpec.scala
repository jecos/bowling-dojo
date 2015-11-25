import Bowling.compute
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class BowlingSpec extends WordSpec {

  "Bowling" when {
    "compute" must {
      "Sum score" in {
        val game: Seq[(Int, Int)] = Seq((1, 5), (3, 6), (2, 6), (5, 4), (7, 2), (9, 0), (3, 5), (6, 3), (8, 1), (4, 5))
        compute(game) shouldBe 85
      }

      "Handle Strike" in {
        val gameWithStrikes: Seq[(Int, Int)] = Seq((10, 0), (6, 2), (5, 4), (7, 2), (9, 0), (10, 0), (3, 6), (1, 4), (5, 4), (2, 3))
        compute(gameWithStrikes) shouldBe 114
      }

      "Handle Last Strike" in {
        val gameWithLastStrike: Seq[(Int, Int)] = Seq((1, 5), (3, 6), (2, 6), (5, 4), (7, 2), (9, 0), (3, 5), (6, 3), (8, 1), (10, 0), (10, 0), (10, 0))
        compute(gameWithLastStrike) shouldBe 106
      }

      "Handle Spare" in {
        val gameWithStrikes: Seq[(Int, Int)] = Seq((4, 6), (6, 2), (5, 4), (7, 2), (9, 0), (7, 3), (3, 6), (1, 4), (5, 4), (2, 3))
        compute(gameWithStrikes) shouldBe 100
      }

      "Handle Last Spare" in {
        val gameWithLastSpare: Seq[(Int, Int)] = Seq((1, 5), (3, 6), (2, 6), (5, 4), (7, 2), (9, 0), (3, 5), (6, 3), (8, 1), (5, 5), (5, 5))
        compute(gameWithLastSpare) shouldBe 96
      }

      "Return 300 for a perfect Game" in {
        val gameWithStrikes: Seq[(Int, Int)] = Seq((10, 0), (10, 0), (10, 0), (10, 0), (10, 0), (10, 0), (10, 0), (10, 0), (10, 0), (10, 0), (10, 0), (10, 0))
        compute(gameWithStrikes) shouldBe 300
      }
    }
  }

}