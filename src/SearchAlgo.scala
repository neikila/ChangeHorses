import scala.collection.mutable

class SearchAlgo(val initField: Field, val endField: Field) {
  type Evristic = Field => Int

  case class State(prev: Option[State], depth: Int, field: Field)(implicit val evr: Evristic) {
    lazy val h: Int = evr(field)
    lazy val f: Int = depth + h

    override def toString: String = s"depth = $depth\nField:\n$field"
  }

  implicit val evristic: Evristic = f => {
    val (white, black) = f.horses.span(_.color == Colors.White)
    val (etalonWhite, etalonBlack) = f.horses.span(_.color == Colors.White)
    white.map(w => etalonWhite.map(_.pos.manhattanDistance(w.pos)).min).sum +
      black.map(w => etalonBlack.map(_.pos.manhattanDistance(w.pos)).min).sum
  }

  private val ordering: Ordering[State] = Ordering.fromLessThan[State]((f, s) => f.f > s.f || (f.f == s.f && f.h < s.h))
  private var checked: Set[State] = Set.empty

  def solve: Option[State] = {
    val open = mutable.PriorityQueue[State](State(None, 0, initField))(ordering)
    var result: Option[State] = None
    var i = 0
    do {
      i += 1
      val head = open.dequeue()
      println(s"i = $i head:\n${head.field}")
      if (head.field.horses.toSet == endField.horses.toSet) result = Some(head)
      else {
        checked = checked + head
        val not = getNextsSorted(head).filterNot(checked.contains)
        println(s"next:\n${not.mkString("\n")}")
        open ++= not
      }
    } while (result.isEmpty && open.nonEmpty)
    result
  }

  private def getNextsSorted(state: State): List[State] = {
    state.field.nonConflictNexts.map(State(Some(state), state.depth + 1, _)).toList
  }
}
