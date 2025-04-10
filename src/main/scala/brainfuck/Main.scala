import scopt.OptionParser

case class Args (autoStepMillisec: Int, source: String)

object Main extends App{

  val source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

  val opts = new OptionParser[Args]("Brainfuck monitor") {
    head("Brainfuck monitor v1.0")

    opt[Int]('a', "autoStepMillisec")
    .optional()
    .action((x, c) => c.copy(autoStepMillisec = x))
    .validate(v => (if (v <= 0) Left("autoStepMillisec must be positive") else Right((): Unit)): Either[String, Unit])

    arg[String]("source")
    .optional()
    .action((x, c) => c.copy(source = x))
  }

  val config = opts.parse(args, Args(0, source))

  val bf = new Brainfuck
  println(bf.parse(config.get.source).flatMap(codes => bf.exec(codes, null)).map(puts => new String(puts.toArray)))
}
