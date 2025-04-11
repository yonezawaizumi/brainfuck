class BrainfuckSpec extends munit.FunSuite {

  test("ハロワ") {
    val source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    val result = (new Brainfuck).run(source, null)
    assert(result.isSuccess)
    assertEquals(new String(result.toOption.get.toArray), "Hello World!\n")
  }

  test("全バイトダンプ") {
    val source = ".+[.+]"
    val result = (new Brainfuck).run(source, null)
    assert(result.isSuccess)
    assertEquals(result.toOption.get.map(_.toInt), (Range(0, 128) ++ Range(-128, 0)).toList)
  }

  test("echo") {
    val source = ",[.,]"
    val input = Range(33, 36).map(_.toByte)
    val result = (new Brainfuck).run(source, input)
    assert(result.isSuccess)
    assertEquals(result.toOption.get, input)
  }

  test("excessive open") {
    val source = "++["
    val result = (new Brainfuck).run(source, null)
    assert(result.isFailure)
    assertEquals(result.toEither.swap.toOption.get.getMessage, "loop overflow")
  }

  test("excessive close") {
    val source = "++][]"
    val result = (new Brainfuck).run(source, null)
    assert(result.isFailure)
    assertEquals(result.toEither.swap.toOption.get.getMessage, "loop underflow")
  }

}
