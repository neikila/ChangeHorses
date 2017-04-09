import Colors.Color

case class Field(size: Point, horses: List[Horse]) {

  def nexts: Stream[Field] = for {
    (horse, index) <- (horses zipWithIndex).toStream
    newPos <- horse.allNextPos.filter(_.isInside)
    if !horses.exists(_.pos == newPos)
  } yield Field(size, horses.updated(index, horse.copy(pos = newPos)))

  def nonConflictNexts: Stream[Field] = nexts.filter(!_.hasConflict)

  def hasConflict: Boolean = horses.zipWithIndex.toStream.exists { case (h, index) =>
    val possible = h.allNextPos.filter(_.isInside)
    val rest = horses.drop(index + 1)
    possible.exists(pos => rest.exists(h2 => h2.pos == pos && h2.color != h.color))
  }

  implicit class PointInField(p: Point) {
    def isInside: Boolean = p.x >= 0 && p.y >= 0 && p.x < size.x && p.y < size.y
  }
//  def isInside(p: Point): Boolean = p.x >= 0 && p.y >= 0 && p.x < size.x && p.y < size.y

  override def toString: String = {
    val strings = for {
      y <- (0 until size.y).reverse
      x <- 0 until size.x
    } yield horses.find(_.pos == Point(x, y)) match {
      case Some(h) => colorStr(h.color)
      case _ => "_"
    }
    strings.sliding(3, 3).map(_.mkString).mkString("\n")
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case f: Field => f.horses.toSet == horses.toSet
    case _ => super.equals(obj)
  }

  private def colorStr(color: Color): String = if (color == Colors.White) "w" else "b"
}
