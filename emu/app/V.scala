
sealed abstract class V {
  def toInt: Int = throw new AssertionError(s"Num expected: $this")
}
object V {
  def bool(b: Boolean) = if(b) True else False

  case class Num(v: Int) extends V {
    override def toInt = v
  }
  case object True extends V
  case object False extends V

  case class F1(name: String) extends V
  case class F2(name: String) extends V
  case class F2X(name: String, x: V) extends V

  case class ModNum(v: Int) extends V

}
