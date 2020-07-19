sealed abstract class Tree
object Tree {
  case class Num(n: Long) extends Tree

  case class F1(name: String) extends Tree
  case class F2(name: String) extends Tree
  case class F3(name: String) extends Tree
  case class Value(v: V) extends Tree
  case class Var(name: String) extends Tree

  case class Ap(f: Tree, x: Tree) extends Tree
}
