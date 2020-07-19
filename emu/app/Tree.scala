sealed abstract class Tree
object Tree {
  case class Num(n: Int) extends Tree

  case class F1(name: String) extends Tree
  case class F2(name: String) extends Tree

  case class Ap(f: Tree, x: Tree) extends Tree
}
