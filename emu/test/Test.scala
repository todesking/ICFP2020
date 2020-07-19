import org.scalatest.funspec.AnyFunSpec

class Test extends AnyFunSpec {
  def assertEval(src: String, expected: V): Unit =
    handle().assertEval(src, expected)

  class Ctx(val send: V => V) {
    val engine = new Engine() {
      override def handleSend(data: V): V = send(data)
    }

    def assertEval(src: String, expected: V): Unit = {
      val debug = false
      if (debug) {
        println(s"src: $src")
        println(s"parsed: ${Parser.parse(src)}")
      }
      assert(engine.evalAll(src) == expected, s"eval($src) should $expected")
    }
    def eval(src: String): V = engine.evalAll(src)
  }
  def handle(send: PartialFunction[V, V] = { x => x }): Ctx =
    new Ctx(send)

  it("Parse fail unless all input is consumed") {
    Parser.parse("ap 1 2") // success
    assertThrows[RuntimeException] { Parser.parse("ap 1") }
  }
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
    assertEval("ap mod 0", V.Mod(V.Num(0)))
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
  it("25. Cons") {
    assertEval("ap ap cons 1 2", V.Cons(V.Num(1), V.Num(2)))
    assertEval("ap ap ap cons 1 2 t", V.Num(1))
    assertEval("ap ap ap cons 1 2 f", V.Num(2))
  }
  it("26. car") {
    assertEval("ap car ap ap cons 1 2", V.Num(1))
  }
  it("27. cdr") {
    assertEval("ap cdr ap ap cons 1 2", V.Num(2))
  }
  it("28. nil") {
    assertEval("ap nil 0", V.True)
  }
  it("29. isnil") {
    assertEval("ap isnil nil", V.True)
    assertEval("ap isnil ap ap cons 1 2", V.False)
  }
  it("30. list") {
    assertEval("()", V.Nil)
    assertEval("(1)", V.Cons(V.Num(1), V.Nil))
    assertEval("(1, 2)", V.Cons(V.Num(1), V.Cons(V.Num(2), V.Nil)))
  }
  it("31. vector") {
    assertEval("ap ap vec 1 2", V.Cons(V.Num(1), V.Num(2)))
  }

  def pic(s: String) =
    s.stripMargin.tail.split("\n").init.map(_.substring(2)).mkString("\n")

  it("32. draw") {
    val p1 = handle().eval("ap draw ( ap ap vec 1 2 )").asInstanceOf[V.Pic]
    //   0    5    A    5
    assert(p1.toString == pic("""
      |0 ___________________
      |1 ___________________
      |2 _*_________________
      |3 ___________________
      |4 ___________________
      |5 ___________________
      |6 ___________________
      |7 ___________________
      |8 ___________________
      |9 ___________________
      |A ___________________
      |B ___________________
      |C ___________________
      |D ___________________
      |E ___________________
    """))
    val p2 = handle().eval(
      "ap draw ( ap ap vec 5 3 , ap ap vec 6 3 , ap ap vec 4 4 , ap ap vec 6 4 , ap ap vec 4 5 )"
    )
    //   0    5    A    5
    assert(p2.toString == pic("""
      |0 ___________________
      |1 ___________________
      |2 ___________________
      |3 _____**____________
      |4 ____*_*____________
      |5 ____*______________
      |6 ___________________
      |7 ___________________
      |8 ___________________
      |9 ___________________
      |A ___________________
      |B ___________________
      |C ___________________
      |D ___________________
      |E ___________________
    """))
  }
  it("33. checkerboard") {
    val p1 = handle().eval("ap draw ap ap checkerboard 3 0")
    //   0    5    A    5
    assert(p1.toString == pic("""
      |0 *_*________________
      |1 _*_________________
      |2 *_*________________
      |3 ___________________
      |4 ___________________
      |5 ___________________
      |6 ___________________
      |7 ___________________
      |8 ___________________
      |9 ___________________
      |A ___________________
      |B ___________________
      |C ___________________
      |D ___________________
      |E ___________________
    """))
    val p2 = handle().eval("ap draw ap ap checkerboard 5 0")
    //   0    5    A    5
    assert(p2.toString == pic("""
      |0 *_*_*______________
      |1 _*_*_______________
      |2 *_*_*______________
      |3 _*_*_______________
      |4 *_*_*______________
      |5 ___________________
      |6 ___________________
      |7 ___________________
      |8 ___________________
      |9 ___________________
      |A ___________________
      |B ___________________
      |C ___________________
      |D ___________________
      |E ___________________
    """))
  }
  it("34. multipledraw") {
    assertEval("ap multipledraw nil", V.Nil)
    val pics = handle().eval(
      s"ap multipledraw ((ap ap vec 0 0), (ap ap vec 1 1))"
    )

    val (p1, p2) = pics match {
      case V.Cons(p1, V.Cons(p2, V.Nil)) =>
        (p1, p2)
      case unk =>
        fail(s"$unk")
    }
    //   0    5    A    5
    assert(p1.toString == pic("""
      |0 *__________________
      |1 ___________________
      |2 ___________________
      |3 ___________________
      |4 ___________________
      |5 ___________________
      |6 ___________________
      |7 ___________________
      |8 ___________________
      |9 ___________________
      |A ___________________
      |B ___________________
      |C ___________________
      |D ___________________
      |E ___________________
    """))
    //   0    5    A    5
    assert(p2.toString == pic("""
      |0 ___________________
      |1 _*_________________
      |2 ___________________
      |3 ___________________
      |4 ___________________
      |5 ___________________
      |6 ___________________
      |7 ___________________
      |8 ___________________
      |9 ___________________
      |A ___________________
      |B ___________________
      |C ___________________
      |D ___________________
      |E ___________________
    """))
  }
  it("35. modulate list") {
    assertEval("ap mod nil", V.Mod(V.Nil))
    assertEval("ap mod (nil)", V.Mod(V.Cons(V.Nil, V.Nil)))
    assertEval("ap mod ap ap cons 1 2", V.Mod(V.Cons(V.Num(1), V.Num(2))))
    assertEval(
      "ap mod (1, 2)",
      V.Mod(V.Cons(V.Num(1), V.Cons(V.Num(2), V.Nil)))
    )
  }
  // 36. send(0)
  it("37. Is 0") {
    assertEval("ap ap ap if0 0 1 2", V.Num(1))
    assertEval("ap ap ap if0 9 1 2", V.Num(2))
  }
  it("38. interact") {
    assertEval("interact", V.F3("interact"))
  }
  it("39. interaction protocol") {
    pending
  }
  it("40. stateless drawing protocol") {
    pending
  }
  it("41. stateful drawing protocol") {
    pending
  }
  it("modulation") {
    assert(V.modulate(V.Num(0)) == "010")
    assert(V.modulate(V.Num(1)) == "01100001")
    assert(V.modulate(V.Num(-1)) == "10100001")
    assert(V.modulate(V.Num(256)) == "011110000100000000")
  }
  it("demodulation") {
    def ok(n: Int) = {
      assert(V.demodulate(V.modulate(V.Num(n))) == V.Num(n))
    }
    ok(0)
    ok(1)
    ok(-1)
    ok(255)
    ok(256)
    ok(-256)
    assert(V.demodulate("0111000010001") == V.Num(17))
  }
}
