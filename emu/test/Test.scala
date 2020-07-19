import org.scalatest.funspec.AnyFunSpec

class Test extends AnyFunSpec {
  def assertEval(src: String, expected: V): Unit =
    handle().assertEval(src, expected)

  class Ctx(val send: V => V) {
    def assertEval(src: String, expected: V): Unit = {
      val engine = new Engine() {
        override def handleSend(data: V): V = send(data)
      }
      val debug = false
      if (debug) {
        println(s"src: $src")
        println(s"parsed: ${Parser.parse(src)}")
      }
      assert(engine.eval(src) == expected, s"eval($src) should $expected")
    }
  }
  def handle(send: PartialFunction[V, V] = { x => x }): Ctx =
    new Ctx(send)

  it("1. nums") {
    assertEval("1", V.Num(1))
  }
  it("3. negative nums") {
    assertEval("-1", V.Num(-1))
  }
  it("5. inc") {
    assertEval("ap inc 0", V.Num(1))
    assertEval("ap inc 100", V.Num(101))
  }
  it("6. dec") {
    assertEval("ap dec 0", V.Num(-1))
  }
  it("7. add") {
    assertEval("ap ap add 1 2", V.Num(3))
  }
  it("9. product") {
    assertEval("ap ap mul 4 2", V.Num(8))
  }
  it("10. div") {
    assertEval("ap ap div 4 2", V.Num(2))
    assertEval("ap ap div 4 3", V.Num(1))
    assertEval("ap ap div 5 -3", V.Num(-1))
    assertEval("ap ap div -5 3", V.Num(-1))
    assertEval("ap ap div -5 -3", V.Num(1))
  }
  it("11. equality and booleans") {
    assertEval("ap ap eq 0 -2", V.False)
    assertEval("ap ap eq 0 0", V.True)
  }
  it("12. strict less-than") {
    assertEval("ap ap lt 1 0", V.False)
    assertEval("ap ap lt 1 1", V.False)
    assertEval("ap ap lt 1 2", V.True)
  }
  it("13. modulate") {
    assertEval("ap mod 0", V.ModNum(0))
  }
  it("14. demodulate") {
    assertEval("ap dem ap mod 0", V.Num(0))
    assertEval("ap dem ap mod 99", V.Num(99))
  }
  it("15. send") {
    handle(send = { case V.Num(1) => V.Num(2) })
      .assertEval("ap send 1", V.Num(2))
  }
  it("16. negate") {
    assertEval("ap neg -1", V.Num(1))
  }
  it("17. function application") {
    val engine = new Engine
    import V.Num
    Seq(
      ("ap inc ap inc 0", Num(2)),
      ("ap inc ap inc ap inc 0", Num(3)),
      ("ap inc ap dec 10", Num(10)),
      ("ap dec ap inc 10", Num(10)),
      ("ap dec ap ap add 10 1", Num(10)),
      ("ap ap add ap ap add 2 3 4", Num(9)),
      ("ap ap add 2 ap ap add 3 4", Num(9)),
      ("ap ap add ap ap mul 2 3 4", Num(10)),
      ("ap ap mul 2 ap ap add 3 4", Num(14))
    ).foreach {
      case (src, expected) =>
        assertEval(src, expected)
    }
  }
  it("18. S combinator") {
    assertEval("ap ap ap s add inc 1", V.Num(3))
    assertEval("ap ap ap s mul ap add 1 6", V.Num(42))
  }
  it("19. C combinator") {
    assertEval("ap ap ap c add 1 2", V.Num(3))
  }
  it("20. B combinator") {
    assertEval("ap ap ap b inc dec 10", V.Num(10))
  }
  it("21. true(K combinator)") {
    assertEval("ap ap t 1 5", V.Num(1))
    assertEval("ap ap t t ap inc 5", V.True)
    assertEval("ap ap t ap inc 5 t", V.Num(6))
  }
  it("22. False") {
    assertEval("ap ap f 1 5", V.Num(5))
  }
  it("23. power of 2") {
    assertEval("ap pwr2 2", V.Num(4))
    assertEval("ap pwr2 3", V.Num(8))
  }
  it("24. I combinator") {
    assertEval("ap i 1", V.Num(1))
  }
  it("24. Cons") {
    assertEval("ap ap cons 1 2", V.Cons(V.Num(1), V.Num(2)))
    assertEval("ap ap ap cons 1 2 t", V.Num(1))
    assertEval("ap ap ap cons 1 2 f", V.Num(2))
  }
  it("25. car") {
    assertEval("ap car ap ap cons 1 2", V.Num(1))
  }
  it("26. cdr") {
    assertEval("ap cdr ap ap cons 1 2", V.Num(2))
  }
}
