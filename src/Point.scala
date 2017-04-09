/**
  * Created by Neikila on 09.04.2017.
  */
case class Point(x: Int, y: Int) {
  def +(p: Point): Point = copy(x + p.x, y + p.y)
}
