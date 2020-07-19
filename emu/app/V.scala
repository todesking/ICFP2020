sealed abstract class V {
  def toInt: Int = throw new AssertionError(s"Num expected: $this")
}
object V {
  def bool(b: Boolean) = if (b) True else False

  case class Num(v: Int) extends V {
    override def toInt = v
  }

  def newPic(w: Int, h: Int) = {
    val data = (0 until h).map { _ =>
      (0 until w).map { _ => false }.toVector
    }.toVector
    Pic(data)
  }

  case class Pic(data: Seq[Seq[Boolean]]) extends V {
    override def toString =
      data
        .map { row =>
          row.map { x => if (x) "*" else "_" }.mkString("")
        }
        .mkString("\n")
    def put(x: Int, y: Int): Pic = {
      val updated = data.updated(y, data(y).updated(x, true))
      copy(data = updated)
    }
  }

  case class Cons(car: V, cdr: V) extends V
  val Nil = F1("nil")

  val True = F2("t")
  val False = F2("f")

  case class F1(name: String) extends V
  case class F2(name: String) extends V
  case class F2X(name: String, x: V) extends V
  case class F3(name: String) extends V
  case class F3X(name: String, x: V) extends V
  case class F3XX(name: String, x1: V, x2: V) extends V

  case class ModNum(v: Int) extends V

}
