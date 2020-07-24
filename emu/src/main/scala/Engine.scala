package icfp2020

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

class Engine {
  private[this] var envTree = Map.empty[String, Tree]
  private[this] var env = Map.empty[String, V]

  var picW = 19
  var picH = 15
  var picOffW = 0
  var picOffH = 0

  var alienProxyEnabled: Boolean = false

  def loadFile(path: String): Unit = {
    val linePat = """(:?[a-z0-9]+)\s*=\s*(.+)""".r
    Files.readAllLines(Paths.get(path)).asScala.filter(_.nonEmpty).foreach {
      case `linePat`(name, src) =>
        define(
          if (name(0) == ':') name else ":" + name,
          src
        )
    }
  }

  def evalAll(src: String): V =
    unwrapAll(eval(src))

  def eval(src: String): V =
    eval(Parser.parse(src))

  def define(name: String, src: String): Unit = {
    envTree = envTree + (name -> Parser.parse(src))
    env = env.removed(name)
  }

  def define(name: String, v: V): Unit = {
    env = env + (name -> v)
  }

  def getVar(name: String): V = env(name)

  def unwrapAll(v: V): V =
    unwrap(v) match {
      case V.Cons(car, cdr) => V.Cons(unwrapAll(car), unwrapAll(cdr))
      case V.Mod(v)         => V.Mod(unwrapAll(v))
      case v                => v
    }

  private[this] val cache = scala.collection.mutable.HashMap.empty[V, V]

  val checkerboard = eval(Parser.parse("""
    ap ap s ap ap b s ap ap c ap ap b c ap ap b ap c ap c
    ap ap s ap ap b s ap ap b ap b ap ap s i i lt eq
    ap ap s mul i nil ap ap s ap ap b s ap ap b ap b cons
    ap ap s ap ap b s ap ap b ap b cons ap c div ap c
    ap ap s ap ap b b ap ap c ap ap b b add neg
    ap ap b ap s mul div ap ap c ap ap b b checkerboard ap ap c add 2
  """))

  val statelessdraw = eval(Parser.parse("""
    ap ap c ap ap b b ap ap b ap b ap cons 0 ap ap c ap ap b b cons ap ap c cons nil ap ap c ap ap b cons ap ap c cons nil nil
  """))
  val statefuldraw = eval(Parser.parse("""
    ap ap b ap b ap ap s ap ap b ap b ap cons 0 ap ap c ap ap b b cons ap ap c cons nil ap ap c cons nil ap c cons
  """))

  import Tree._
  def eval(tree: Tree): V =
    unwrap(evalLazy(tree))

  def evalLazy(tree: Tree): V =
    tree match {
      case Var(name)  => V.LazyRef(name)
      case Value(v)   => v
      case Num(n)     => V.Num(n)
      case F1(f)      => V.F1(f)
      case F2(f)      => V.F2(f)
      case F3(f)      => V.F3(f)
      case Ap(tf, tx) => V.LazyApp(evalLazy(tf), evalLazy(tx))
    }

  def unwrapInt(v: V): Long =
    unwrap(v) match {
      case V.Num(n) => n
      case unk      => throw new RuntimeException(s"Int required: $unk")
    }
  def unwrap(v: V): V =
    cache.getOrElseUpdate(
      v,
      v match {
        case V.LazyApp(f, t) =>
          val a = evalApp(f, t)
          a match {
            case V.LazyApp(f, t) => throw new AssertionError(s"$v")
            case _               => a
          }
        case V.LazyRef(name) =>
          val tree = envTree.getOrElse(
            name,
            throw new RuntimeException(s"Unbound var: $name")
          )
          eval(tree)
        case v =>
          v
      }
    )

  def evalApp(f: V, x: V): V =
    unwrap(f) match {
      case V.F1(name) =>
        name match {
          case "inc" => V.Num(unwrapInt(x) + 1)
          case "dec" => V.Num(unwrapInt(x) - 1)
          case "mod" => V.Mod(x)
          case "dem" =>
            unwrap(x) match {
              case V.Mod(v) => unwrap(v)
              case unk      => throw new RuntimeException(s"ModNum expected: $x")
            }
          case "send" => handleSend(unwrap(x))
          case "neg"  => V.Num(-unwrapInt(x))
          case "pwr2" =>
            val n = unwrapInt(x)
            if (n < 0) throw new RuntimeException(s"pwr2: n >= 0 required: $n")
            else V.Num(1 << n.toInt)
          case "i" =>
            unwrap(x)
          case "car" =>
            unwrap(x) match {
              case V.Cons(car, cdr) => unwrap(car)
              case f                => evalApp(f, V.True)
            }
          case "cdr" =>
            unwrap(x) match {
              case V.Cons(car, cdr) => unwrap(cdr)
              case f                => evalApp(f, V.False)
            }
          case "nil" =>
            V.True
          case "isnil" =>
            unwrap(x) match {
              case V.Nil        => V.True
              case V.Cons(_, _) => V.False
              case unk =>
                throw new NotImplementedError("isnil <unknown-closure>")
            }
          case "draw" =>
            def draw(pic: V.Pic, l: V): V.Pic =
              unwrap(l) match {
                case V.Cons(car, cdr) =>
                  val p2 = unwrap(car) match {
                    case V.Cons(x, y) =>
                      pic.put(unwrapInt(x), unwrapInt(y))
                    case unk =>
                      throw new NotImplementedError(
                        s"(Int, Int) required: $unk"
                      )
                  }
                  draw(p2, cdr)
                case V.Nil =>
                  pic
                case unk =>
                  throw new NotImplementedError(
                    s"List[(Int, Int)] required: $unk"
                  )
              }
            val init = V.newPic(picW, picH, picOffW, picOffH)
            draw(init, x)
          case "multipledraw" =>
            def go(x: V): V = {
              unwrap(x) match {
                case V.Cons(car, cdr) =>
                  V.Cons(
                    eval(Tree.Ap(Tree.F1("draw"), Tree.Value(car))),
                    go(cdr)
                  )
                case V.Nil =>
                  V.Nil
                case unk =>
                  throw new NotImplementedError(s"List required: $unk")
              }
            }
            go(x)
          case unk => throw new AssertionError(s"Unknown F1 name: $unk")
        }
      case V.F2(name) => V.F2X(name, x)
      case V.F2X(name, x1) =>
        name match {
          case "add" =>
            V.Num(unwrapInt(x1) + unwrapInt(x))
          case "mul" =>
            V.Num(unwrapInt(x1) * unwrapInt(x))
          case "div" =>
            V.Num(unwrapInt(x1) / unwrapInt(x))
          case "eq" =>
            V.bool(unwrap(x1) == unwrap(x))
          case "lt" =>
            V.bool(unwrapInt(x1) < unwrapInt(x))
          case "t" =>
            unwrap(x1)
          case "f" =>
            unwrap(x)
          case "cons" | "vec" =>
            V.Cons(x1, x)
          case "checkerboard" =>
            evalApp(evalApp(checkerboard, x1), x)
          case unk =>
            throw new AssertionError(s"Unknown F2 name: $unk")
        }
      case V.F3(name)      => V.F3X(name, x)
      case V.F3X(name, x1) => V.F3XX(name, x1, x)
      case V.F3XX(name, x0, x1) =>
        name match {
          case "s" => evalApp(evalApp(x0, x), V.LazyApp(x1, x))
          case "c" => evalApp(evalApp(x0, x), x1)
          case "b" => evalApp(x0, V.LazyApp(x1, x))
          case "if0" =>
            unwrap(x0) match {
              case V.Num(0) => x1
              case _        => x
            }
          case unk => throw new AssertionError(s"Unknown F3 name: $unk")
        }
      case V.Cons(x0, x1) =>
        evalApp(evalApp(x, x0), x1)
      case unk =>
        throw new RuntimeException(s"Function required: $f")
    }

  def evalToInt(tree: Tree): Long =
    eval(tree) match {
      case V.Num(n) => n
      case x        => throw new RuntimeException(s"Int required but $x: eval($tree)")
    }

  def handleSend(data: V): V =
    data match {
      case V.Mod(v) =>
        val payload = V.modulate(v)
        V.demodulate(alienProxy(payload))
      case unk => throw new RuntimeException(s"Modulated value required: $data")
    }
  def alienProxy(data: String): String =
    if (alienProxyEnabled) AlienProxy.send(data)
    else throw new RuntimeException(s"Alien proxy is disabled")
}
