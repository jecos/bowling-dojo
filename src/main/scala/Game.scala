class Frame(val throw1: Int, val throw2: Int)

object Frame {
  def apply(throw1: Int, throw2: Int = 0): Frame = new Frame(throw1, throw2)

  def unapply(frame: Frame): Option[(Int, Int)] = {
    Some((frame.throw1, frame.throw2))
  }

}

object Strike {
  def apply(): Frame = Frame(10)

  def unapply(frame: Frame): Boolean = if (frame.throw1 == 10) true else false
}

object Spare {
  def apply(throw1: Int): Frame = Frame(throw1, 10 - throw1)

  def unapply(frame: Frame): Option[Int] = if (frame.throw1 + frame.throw2 == 10) Some(frame.throw1) else None
}


object Game {

  val printFrame: PartialFunction[Frame, String] = {
    case _@Strike() => "X"
    case Spare(0) => "-/"
    case Spare(throw1) => s"$throw1/"
    case Frame(0, throw2) if throw2 != 0 => s"-$throw2"
    case Frame(throw1, 0) if throw1 != 0 => s"$throw1-"
    case f => s"${f.throw1}${f.throw2}"
  }

  def printNormal(frames: Seq[Frame]): String = frames.map(printFrame).mkString("|")

  def print(frames: Seq[Frame]): String = {
    if (frames.length <= 10) {
      printNormal(frames)
    } else {
      val (normal, bonuses) = frames.splitAt(10)
      val printedBonuses = normal.last match {
        case _@Strike() if bonuses.nonEmpty => bonuses.take(2).map(printFrame).mkString("")
        case Spare(_) if bonuses.nonEmpty => bonuses.head.throw1.toString
        case _ => ""
      }
      printNormal(normal) + "||" + printedBonuses
    }

  }

}
