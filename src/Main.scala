/**
  * Created by Neikila on 09.04.2017.
  */
object Main {
  def main(args: Array[String]): Unit = {
    val point: Point = Point(3, 3)

    val field = Field(point, Horse(Point(1, 0), Colors.White) :: Horse(Point(1, 2), Colors.Black) :: Nil)
    val f2 = Field(point, Horse(Point(1, 2), Colors.White) :: Horse(Point(1, 0), Colors.Black) :: Nil)

    val sa: Option[SearchAlgo#State] = new SearchAlgo(field, f2).solve
//    println(field)
    println
    println
    println
    println("Result:")
    sa.foreach(printResult)
  }

  def printResult(state: SearchAlgo#State): Unit = {
    state.prev.foreach(printResult)
    println(s"step:${state.depth}")
    println(state.field)
    println
  }
}
