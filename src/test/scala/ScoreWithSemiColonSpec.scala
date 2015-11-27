import ScoreWithSemiColon.compute

import org.scalatest.Matchers._
import org.scalatest.WordSpec

class ScoreWithSemiColonSpec extends WordSpec {

  "ScoreWithSemiColon" when {
    "compute" must {
      "Sum score" in {
        val game: Seq[Int] = Seq(1, 5, 3, 6, 2, 6, 5, 4, 7, 2, 9, 0, 3, 5, 6, 3, 8, 1, 4, 5)
        compute(game) shouldBe 85
      }

      "Handle Strike" in {
        val gameWithStrikes: Seq[Int] = Seq(10, 3, 6, 2, 6, 5, 4, 7, 2, 9, 0, 3, 5, 6, 3, 8, 1, 4, 5)
        compute(gameWithStrikes) shouldBe 98
      }

      "Handle Last Strike" in {
        val gameWithLastStrike: Seq[Int] = Seq(1, 5, 3, 6, 2, 6, 5, 4, 7, 2, 9, 0, 3, 5, 6, 3, 8, 1, 10, 4, 3)
        compute(gameWithLastStrike) shouldBe 93
      }

      "Handle Spare" in {
        val gameWithSpares: Seq[Int] = Seq(5, 5, 3, 6, 2, 6, 5, 4, 7, 2, 9, 0, 3, 5, 6, 3, 8, 1, 4, 5)
        compute(gameWithSpares) shouldBe 92
      }

      "Handle Last Spare" in {
        val gameWithLastSpare: Seq[Int] = Seq(1, 5, 3, 6, 2, 6, 5, 4, 7, 2, 9, 0, 3, 5, 6, 3, 8, 1, 5, 5, 1)
        compute(gameWithLastSpare) shouldBe 87
      }

      "Return 300 for a perfect Game" in {
        val gameWithStrikes: Seq[Int] = Seq(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
        compute(gameWithStrikes) shouldBe 300
      }

      "Return 150 for 5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5" in {
        val gameWithStrikes: Seq[Int] = Seq(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
        compute(gameWithStrikes) shouldBe 150
      }

      "Return 167 for X|7/|9-|X|-8|8/|-6|X|X|X||81" in {
        val gameWithStrikes: Seq[Int] = Seq(10, 7, 3, 9, 0, 10, 0, 8, 8, 2, 0, 6, 10, 10, 10, 8, 1)
        compute(gameWithStrikes) shouldBe 167
      }

      "Return 90 for 9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||" in {
        val gameWithStrikes: Seq[Int] = Seq(9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0)
        compute(gameWithStrikes) shouldBe 90
      }

      "Return 110 for 0/|0/|0/|0/|0/|0/|0/|0/|0/|0/||X" in {
        val gameWithStrikes: Seq[Int] = Seq(0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 10, 10)
        compute(gameWithStrikes) shouldBe 110
      }
    }

  }

}