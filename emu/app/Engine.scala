class Engine {
  def eval(src: String): V =
    eval(Parser.parse(src))

  import Tree._
  def eval(tree: Tree): V =
    tree match {
      case Num(n) => V.Num(n)
      case F1(f)  => V.F1(f)
      case F2(f)  => V.F2(f)
      case F3(f)  => V.F3(f)
      case Ap(tf, tx) =>
        evalApp(eval(tf), eval(tx))
    }

  def evalApp(f: V, x: V): V =
    f match {
      case V.F1(name) =>
        name match {
          case "inc" => V.Num(x.toInt + 1)
          case "dec" => V.Num(x.toInt - 1)
          case "mod" => V.ModNum(x.toInt)
          case "dem" =>
            x match {
              case V.ModNum(n) => V.Num(n)
              case unk         => throw new RuntimeException(s"ModNum expected: $x")
            }
          case "send" => handleSend(x)
          case "neg"  => V.Num(-x.toInt)
          case "pwr2" =>
            val n = x.toInt
            if (n < 0) throw new RuntimeException(s"pwr2: n >= 0 required: $n")
            else V.Num(1 << n)
          case "i" =>
            x
          case "car" =>
            x match {
              case V.Cons(car, cdr) => car
              case f                => evalApp(f, V.True)
            }
          case "cdr" =>
            x match {
              case V.Cons(car, cdr) => cdr
              case f                => evalApp(f, V.False)
            }
          case "nil" =>
            V.True
          case unk => throw new AssertionError(s"Unknown F1 name: $unk")
        }
      case V.F2(name) => V.F2X(name, x)
      case V.F2X(name, x1) =>
        name match {
          case "add" =>
            V.Num(x1.toInt + x.toInt)
          case "mul" =>
            V.Num(x1.toInt * x.toInt)
          case "div" =>
            V.Num(x1.toInt / x.toInt)
          case "eq" =>
            V.bool(x1 == x)
          case "lt" =>
            V.bool(x1.toInt < x.toInt)
          case "t" =>
            x1
          case "f" =>
            x
          case "cons" =>
            V.Cons(x1, x)
          case unk =>
            throw new AssertionError(s"Unknown F2 name: $unk")
        }
      case V.F3(name)      => V.F3X(name, x)
      case V.F3X(name, x1) => V.F3XX(name, x1, x)
      case V.F3XX(name, x0, x1) =>
        name match {
          case "s" => evalApp(evalApp(x0, x), evalApp(x1, x))
          case "c" => evalApp(evalApp(x0, x), x1)
          case "b" => evalApp(x0, evalApp(x1, x))
          case unk => throw new AssertionError(s"Unknown F3 name: $unk")
        }
      case V.Cons(x0, x1) =>
        evalApp(evalApp(x, x0), x1)
      case unk =>
        throw new RuntimeException(s"Function required: $f")
    }

  def evalToInt(tree: Tree): Int =
    eval(tree) match {
      case V.Num(n) => n
      case x        => throw new RuntimeException(s"Int required but $x: eval($tree)")
    }

  def handleSend(data: V): V = data
}
