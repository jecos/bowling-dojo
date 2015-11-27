import org.scalatest.Matchers._
import org.scalatest.WordSpec

class GameSpec extends WordSpec {

  "Game" when {
    "print" must {
      "display 12|34|25" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Frame(2, 5))
        Game.print(game) shouldBe "12|34|25"
      }

      "display 12|34|25|X" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Frame(2, 5), Strike())
        Game.print(game) shouldBe "12|34|25|X"
      }

      "also display 12|34|25|X" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Frame(2, 5), Frame(10, 0))
        Game.print(game) shouldBe "12|34|25|X"
      }

      "display 12|34|25|4/" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Frame(2, 5), Spare(4))
        Game.print(game) shouldBe "12|34|25|4/"
      }

      "also display 12|34|25|4/" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Frame(2, 5), Frame(4, 6))
        Game.print(game) shouldBe "12|34|25|4/"
      }

      "display 12|34|25|X|4/" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Frame(2, 5), Strike(), Spare(4))
        Game.print(game) shouldBe "12|34|25|X|4/"
      }

      "display 12|34|2-" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Frame(2, 0))
        Game.print(game) shouldBe "12|34|2-"
      }

      "display 12|34|-/" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Spare(0))
        Game.print(game) shouldBe "12|34|-/"
      }
      "also display 12|34|-/" in {
        val game: Seq[Frame] = Seq(Frame(1, 2), Frame(3, 4), Frame(0, 10))
        Game.print(game) shouldBe "12|34|-/"
      }

      "display 5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5" in {
        val game: Seq[Frame] = (for {t <- 1 to 10} yield Spare(5)) ++ Seq(Frame(5, 0))
        Game.print(game) shouldBe "5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5"
      }

      "display X|X|X|X|X|X|X|X|X|X||XX" in {
        val game: Seq[Frame] = for {t <- 1 to 12} yield Strike()
        Game.print(game) shouldBe "X|X|X|X|X|X|X|X|X|X||XX"
      }

      "display X|X|X|X|X|X|X|X|X|X||X" in {
        val game: Seq[Frame] = for {t <- 1 to 11} yield Strike()
        Game.print(game) shouldBe "X|X|X|X|X|X|X|X|X|X||X"
      }
      "display X|X|X|X|X|X|X|X|X|X||34" in {
        val game: Seq[Frame] = (for {t <- 1 to 10} yield Strike()) ++ Seq(Frame(3, 4))
        Game.print(game) shouldBe "X|X|X|X|X|X|X|X|X|X||34"
      }
    }
  }

}