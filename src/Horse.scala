import Colors.Color

/**
  * Created by Neikila on 09.04.2017.
  */
case class Horse(pos: Point, color: Color) {
  private val l1: Stream[Int] = 1 #:: -1 #:: Stream.empty
  private val l2: Stream[Int] = 2 #:: -2 #:: Stream.empty
  private val combo = (l1, l2) #:: (l2, l1) #:: Stream.empty

  def allNextPos: Stream[Point] = for {
    (firsts, seconds) <- combo
    first <- firsts
    second <- seconds
  } yield pos + Point(first, second)
}
