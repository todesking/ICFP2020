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
}

object Comm {
  lazy val galaxied = {
    val engine = new Engine
    engine.loadFile("./galaxy.txt")
    engine.alienProxyEnabled = true
    new Comm(engine)
  }
}
