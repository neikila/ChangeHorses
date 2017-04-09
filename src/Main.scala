/**
  * Created by Neikila on 09.04.2017.
  */
object Main {
  def main(args: Array[String]): Unit = {
    val point: Point = Point(3, 4)
    val field = Field(point,
      Horse(Point(1, 0), Colors.White) :: Horse(Point(2, 0), Colors.White) :: Horse(Point(0, 0), Colors.White) ::
        Horse(Point(1, 3), Colors.Black) :: Horse(Point(2, 3), Colors.Black) :: Horse(Point(0, 3), Colors.Black) :: Nil)
    val f2 = Field(point,
      Horse(Point(1, 0), Colors.Black) :: Horse(Point(2, 0), Colors.Black) :: Horse(Point(0, 0), Colors.Black) ::
        Horse(Point(1, 3), Colors.White) :: Horse(Point(2, 3), Colors.White) :: Horse(Point(0, 3), Colors.White) :: Nil)

    val sa: Option[SearchAlgo#State] = new SearchAlgo(field, f2).solve
    println
    println
    println
    println("Result:")
    new SearchAlgo(field, f2).test
    sa.foreach(printResult)
  }

  def printResult(state: SearchAlgo#State): Unit = {
    state.prev.foreach(printResult)
    println(s"step:${state.depth}")
    println(state.field)
    println
  }
}
