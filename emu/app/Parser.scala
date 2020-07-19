object Parser {
  case class Ctx(data: Seq[String], var index: Int) {
    def head = data(index)
    def isEof = index == data.size
    def next(n: Int) =
      if (index + n <= data.size) index = index + n
      else throw new RuntimeException(s"Eof reached: $index + $n")

    val ReNum = """(-?[0-9]+)""".r
    def parse(): Tree =
      head match {
        case ReNum(n) =>
          index += 1
          Tree.Num(n.toInt)
        case name @ ("inc" | "dec" | "mod" | "dem" | "send" | "neg" | "pwr2" |
            "i" | "car" | "cdr" | "nil") =>
          index += 1
          Tree.F1(name)
        case name @ ("add" | "mul" | "div" | "eq" | "lt" | "t" | "f" |
            "cons") =>
          index += 1
          Tree.F2(name)
        case name @ ("s" | "c" | "b") =>
          index += 1
          Tree.F3(name)
        case "ap" =>
          index += 1
          val f = parse()
          val x = parse()
          Tree.Ap(f, x)
      }
  }

  def parse(src: String): Tree =
    Ctx(src.split("""\s+""").toSeq, 0).parse()
}
