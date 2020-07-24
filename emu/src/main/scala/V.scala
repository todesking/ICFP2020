package icfp2020

sealed abstract class V {}
object V {
  def bool(b: Boolean) = if (b) True else False

  case class Num(v: Long) extends V {
    override def toString = s"$v"
  }

  def newPic(w: Int, h: Int, offW: Int, offH: Int) = {
    val data = (0 until h).map { _ =>
      (0 until w).map { _ => false }.toVector
    }.toVector
    Pic(data, offW, offH)
  }

  case class Pic(data: Seq[Seq[Boolean]], offW: Int, offH: Int) extends V {
    require(data.nonEmpty)

    def height = data.size
    def width = data(0).size

    def xmin = -offW
    def xmax = width - offW - 1
    def ymin = -offH
    def ymax = height - offH - 1

    override def toString =
      data
        .map { row =>
          row.map { x => if (x) "*" else "_" }.mkString("")
        }
        .mkString("\n")

    def put(x: Int, y: Int): Pic = {
      val absX = x + offW
      val absY = y + offH
      val margin = 10
      val (newOffW, newW) =
        if(absX < 0)  (offW + -absX + margin / 2, width + -absX + margin)
        else if(absX >= width) (offW, absX + margin)
        else (offW, width)
      val (newOffH, newH) =
        if(absY < 0) (offH + -absY + margin / 2, height + -absY + margin)
        else if(absY >= height) (offH, absY + margin)
        else (offH, height)
      if(width < newW || height < newH) expand(offW, newW, newOffH, newH).putInternal(x, y)
      else putInternal(x, y)
    }

    def get(x: Int, y: Int): Boolean =
      data(y + offH)(x + offW)

    private[this] def expand(newOffW: Int, newW: Int, newOffH: Int, newH: Int) = {
      require(width <= newW && height <= newH)
      val newData =
        (0 until newH).view.map { ih =>
          (0 until newW).view.map { iw =>
            val x = iw + newOffW
            val y = ih + newOffH
            if(xmin <= x && x <= xmax && ymin <= y && y <= ymax) get(x, y)
            else false
          }.toVector
        }.toVector
      copy(newData, newOffW, newOffH)
    }

    private def putInternal(x: Int, y: Int) = putAbs(x + offW, y + offH)

    private[this] def putAbs(x: Long, y: Long): Pic = {
      val updated = data.updated(y.toInt, data(y.toInt).updated(x.toInt, true))
      copy(data = updated)
    }
  }

  case class Cons(car: V, cdr: V) extends V
  val Nil = F1("nil")

  val True = F2("t")
  val False = F2("f")

  case class F1(name: String) extends V {
    override def toString = s"$name:1"
  }
  case class F2(name: String) extends V {
    override def toString = s"$name:2"
  }
  case class F2X(name: String, x: V) extends V {
    override def toString = s"$name($x, _)"
  }
  case class F3(name: String) extends V {
    override def toString = s"$name:3"
  }
  case class F3X(name: String, x: V) extends V {
    override def toString = s"$name($x, _, _)"
  }
  case class F3XX(name: String, x1: V, x2: V) extends V {
    override def toString = s"$name($x1, $x2, _)"
  }

  case class LazyApp(f: V, x: V) extends V
  case class LazyRef(name: String) extends V

  case class Mod(v: V) extends V {
    override def toString = s"mod($v)"
  }

  def modulate(v: V): String =
    v match {
      case V.Num(n) =>
        val abs = if (n < 0) -n else n
        val data = if (n == 0) "" else java.lang.Long.toString(abs, 2)
        val padding =
          if (data.size % 4 == 0) ""
          else (0 until (4 - data.size % 4)).map { _ => "0" }.mkString("")

        val bodyPart = padding + data
        val len = bodyPart.size / 4
        val sigPart = if (n >= 0) "01" else "10"
        val lenPart = (0 until len).map { _ => "1" }.mkString("") + "0"

        sigPart + lenPart + bodyPart
      case V.Nil            => "00"
      case V.Cons(car, cdr) => "11" + modulate(car) + modulate(cdr)
      case unk              => throw new RuntimeException(s"Can't modulate $v")
    }

  def demodulate(s: String) = new ModParser(s, 0).parse()
  class ModParser(s: String, var i: Int) {
    def peek(n: Int) = s(i + n)
    def peek(n: Int, l: Int) = s.substring(i + n, i + n + l)
    def move(n: Int) = {
      i += n
    }
    def parse(): V = {
      if (peek(0) == '0' && peek(1) == '1') {
        move(2)
        V.Num(parseAbs())
      } else if (peek(0) == '1' && peek(1) == '0') {
        move(2)
        V.Num(-1 * parseAbs())
      } else if (peek(0) == '0' && peek(1) == '0') {
        move(2)
        V.Nil
      } else if (peek(0) == '1' && peek(1) == '1') {
        move(2)
        val car = parse()
        val cdr = parse()
        V.Cons(car, cdr)
      } else {
        throw new RuntimeException(s"Demodulate failed at $i: s=$s")
      }
    }

    def parseAbs() = {
      var len = 0
      while (peek(0) == '1') {
        move(1)
        len += 1
      }
      move(1) // trailing 0
      if (len == 0) 0
      else {
        val n = Integer.parseInt(peek(0, len * 4), 2)
        move(len * 4)
        n
      }
    }
  }

  def toSeq(v: V): List[V] =
    v match {
      case V.Cons(car, cdr) =>
        car :: toSeq(cdr)
      case V.Nil => scala.collection.immutable.Nil
      case unk   => throw new RuntimeException(s"List required: $unk")
    }

  def isProperList(v: V): Boolean =
    v match {
      case Nil        => true
      case Cons(l, r) => isProperList(r)
      case _          => false
    }

  def pretty(v: V): String =
    v match {
      case Cons(l, r) if isProperList(v) =>
        toSeq(v).map(pretty).mkString("(", ", ", ")")
      case v => v.toString
    }
}
