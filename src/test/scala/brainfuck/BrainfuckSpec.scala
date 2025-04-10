class BrainfuckSpec extends munit.FunSuite {

  test("ハロワ") {
    val source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    val result = (new Brainfuck).run(source, null)
    assert(result.isRight)
    assertEquals(new String(result.toOption.get.toArray), "Hello World!\n")
  }

  test("全バイトダンプ") {
    val source = ".+[.+]"
    val result = (new Brainfuck).run(source, null)
    assert(result.isRight)
    assertEquals(result.toOption.get.map(_.toInt), (Range(0, 128) ++ Range(-128, 0)).toList)
  }

  test("echo") {
    val source = ",[.,]"
    val input = Range(33, 36).map(_.toByte)
    val result = (new Brainfuck).run(source, input)
    assert(result.isRight)
    assertEquals(result.toOption.get, input)
  }

}
