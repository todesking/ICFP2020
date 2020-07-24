package icfp2020

class Comm(val engine: Engine) {
  def interact(protocol: V, state: V, vector: V): (V, Seq[V]) = {
    engine.unwrapAll(
      engine.evalApp(engine.evalApp(protocol, state), vector)
    ) match {
      case V.Cons(flag, V.Cons(newState, V.Cons(data, V.Nil))) =>
        val multipledraw = Tree.Ap(Tree.F1("multipledraw"), Tree.Value(data))
        if (flag == V.Num(0))
          (newState, V.toSeq(engine.unwrapAll(engine.eval(multipledraw))))
        else interact(protocol, newState, engine.handleSend(V.Mod(data)))
      case unk => throw new RuntimeException(s"Unexpected value: $unk")
    }
  }

  class Ctx(protocol: V, state: V) {
    def apply(x: Int, y: Int): Ctx =
      interactLoop(protocol, state, V.Cons(V.Num(x), V.Num(y)))
  }
  def interactLoop(protocol: V, state: V, vector: V): Ctx = {
    val (newState, res) = interact(protocol, state, vector)
    res.foreach { pic =>
      println(pic)
      println()
    }
    new Ctx(protocol, state)
  }
}

object Comm {
  lazy val galaxied = loadGalaxied()

  def loadGalaxied(w: Int = 80, h: Int = 10) = {
    val engine = new Engine
    engine.loadFile("./galaxy.txt")
    engine.alienProxyEnabled = true
    engine.picW = w
    engine.picH = h
    engine.picOffW = engine.picW / 2
    engine.picOffH = engine.picH / 2
    new Comm(engine)
  }
}
