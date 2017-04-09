
case class Field(size: Point, horses: List[Point]) {

  case class Horse(pos: Point) {
    private val l1: Stream[Int] = 1 #:: -1 #:: Stream.empty
    private val l2: Stream[Int] = 2 #:: -2 #:: Stream.empty
    private val combo = (l1, l2) #:: (l2, l1) #:: Stream.empty

    def allNextPos: Stream[Point] = for {
      (firsts, seconds) <- combo
      first <- firsts
      second <- seconds
    } yield pos + Point(first, second)

    def possible: Stream[Point] = allNextPos.filter(_.isInside)
  }

  def nexts: Stream[Field] = {
    for {
      (horse, index) <- (horses zipWithIndex).toStream
      horseMoved <- Horse(horse).possible
      if !horses.contains(horseMoved)
    } yield Field(size, horses.updated(index, horseMoved))
  }

  implicit class PointInField(p: Point) {
    def isInside: Boolean = p.x > 0 && p.y > 0 && p.x < size.x && p.y < size.y
  }
}
