/**
  * Created by Neikila on 09.04.2017.
  */
object Main {
  def main(args: Array[String]): Unit = {
    println(Field(Point(9, 9), Point(0, 0) :: Point(1, 2) :: Nil).nexts.mkString("\n"))
  }
}
