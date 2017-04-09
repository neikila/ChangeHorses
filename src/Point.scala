/**
  * Created by Neikila on 09.04.2017.
  */
case class Point(x: Int, y: Int) {
  def +(p: Point): Point = copy(x + p.x, y + p.y)
  def -(p: Point): Point = copy(x - p.x, y - p.y)

  def abs: Point = copy(math.abs(x), math.abs(y))
  def sum: Int = x + y

  def manhattanDistance(p: Point): Int = (this - p).abs.sum
}
