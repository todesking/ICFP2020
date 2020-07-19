object Parser {
  case class Ctx(data: Seq[String], var index: Int) {
    def isEof = index == data.size
    def move(n: Int) =
      if (index + n <= data.size) index = index + n
      else throw new RuntimeException(s"Eof reached: $index + $n")
    def peek(n: Int) =
      data(index + n)

    def parseToEnd(): Tree = {
      val t = parse()
      if (index < data.size)
        throw new RuntimeException(s"Input not all consumed($index)")
      else t
    }

    val ReNum = """(-?[0-9]+)""".r
    def parse(): Tree =
      peek(0) match {
        case name if name.nonEmpty && name(0) == ':' =>
          move(1)
          Tree.Var(name)
        case ReNum(n) =>
          move(1)
          Tree.Num(n.toLong)
        case name @ ("inc" | "dec" | "mod" | "dem" | "send" | "neg" | "pwr2" |
            "i" | "car" | "cdr" | "nil" | "isnil" | "draw" | "multipledraw") =>
          move(1)
          Tree.F1(name)
        case name @ ("add" | "mul" | "div" | "eq" | "lt" | "t" | "f" | "cons" |
            "vec" | "checkerboard") =>
          move(1)
          Tree.F2(name)
        case name @ ("s" | "c" | "b" | "if0" | "interact") =>
          index += 1
          Tree.F3(name)
        case "ap" =>
          move(1)
          val f = parse()
          val x = parse()
          Tree.Ap(f, x)
        case "(" =>
          move(1)
          var l = Vector.empty[Tree]
          while (peek(0) != ")") {
            if (l.nonEmpty) {
              if (peek(0) != ",")
                throw new RuntimeException(
                  s"Parser: comma expected: ${peek(0)}"
                )
              move(1)
            }
            l = l :+ parse()
          }
          move(1) // rparen
          l.foldRight[Tree](Tree.F1("nil")) { (x, l) =>
            Tree.Ap(
              Tree.Ap(
                Tree.F2("cons"),
                x
              ),
              l
            )
          }
      }
  }

  def parse(src: String): Tree = {
    val s = src
      .replace("(", " ( ")
      .replace(")", " ) ")
      .replace(",", " , ")
    Ctx(s.split("""\s+""").toSeq.filter(_.nonEmpty), 0).parseToEnd()
  }
}
