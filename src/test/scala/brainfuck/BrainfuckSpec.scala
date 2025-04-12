import scala.util.Try

class BrainfuckSpec extends munit.FunSuite {

  test("ハロワ") {
    val source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    (new Brainfuck).run(source, null, (result : Try[Seq[Byte]]) => {
      assert(result.isSuccess)
      assertEquals(new String(result.toOption.get.toArray), "Hello World!\n")
    })
  }

  test("全バイトダンプ") {
    val source = ".+[.+]"
    (new Brainfuck).run(source, null, (result : Try[Seq[Byte]]) => {
      assert(result.isSuccess)
      assertEquals(result.toOption.get.map(_.toInt), (Range(0, 128) ++ Range(-128, 0)).toList)
    })
  }

  test("echo") {
    val source = ",[.,]"
    val input = Range(33, 36).map(_.toByte)
    (new Brainfuck).run(source, input, (result : Try[Seq[Byte]]) => {
      assert(result.isSuccess)
      assertEquals(result.toOption.get, input)
    })
  }

  test("excessive open") {
    val source = "++["
    (new Brainfuck).run(source, null, (result : Try[Seq[Byte]]) => {
      assert(result.isFailure)
      assertEquals(result.toEither.swap.toOption.get.getMessage, "loop overflow")
    })
  }

  test("excessive close") {
    val source = "++][]"
    (new Brainfuck).run(source, null, (result : Try[Seq[Byte]]) => {
     assert(result.isFailure)
      assertEquals(result.toEither.swap.toOption.get.getMessage, "loop underflow")
    })
  }

}
