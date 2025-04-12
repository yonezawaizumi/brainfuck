import net.team2xh.scurses._
import scala.collection.immutable._
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util._
import scopt.OptionParser

class HexDumpedStrings(val chars: String, val hexes: String)

object HexDumpedStrings {
  def fromBytes(bytes: Seq[Byte]) = bytes.foldLeft(new HexDumpedStrings("", ""))((hds, b) => new HexDumpedStrings(
    hds.chars + " " + (if (32 <= b && b <= 126) " " + b.toChar else "--"),
    hds.hexes + f" $b%02x"
  ))
}

object PointerString {
  def format(pos: Int, maxSize: Int, mul: Int = 1, pointerChar: Char = '^') = "".padTo(pos * mul - 1, ' ') + pointerChar + "".padTo((maxSize - pos) * mul, ' ')
}

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

  val bf = new Brainfuck
  val config = opts.parse(args, Args(0, source)).get

  Scurses { screen =>

    val (w, h) = screen.size

    def dumpState(source: String, state :State[SeqIOStream]) : Unit = {
      val it = Iterator.continually(0).zipWithIndex.map(_._2)
      // NOTE: ステップ数
      screen.put(0, it.next, state.step.toString + " steps")
      it.next
      // NOTE: メモリーダンプ
      screen.put(0, it.next, "heap")
      val heap = HexDumpedStrings.fromBytes(state.machine.heap)
      screen.put(0, it.next, heap.chars)
      screen.put(0, it.next, heap.hexes)
      screen.put(0, it.next, PointerString.format(state.machine.ptr, state.machine.heap.length, 3))
      it.next
      // NOTE: 出力ダンプ
      screen.put(0, it.next, "outputs")
      val out = HexDumpedStrings.fromBytes(state.io.out.reverse)
      screen.put(0, it.next, out.chars)
      screen.put(0, it.next, out.hexes)
      it.next
      // NOTE: コードダンプ
      screen.put(0, it.next, "codes")
      screen.put(0, it.next, source)
      screen.put(0, it.next, PointerString.format(state.machine.pos, source.length))
    }

    bf.parse(config.source) match {
      case Failure(e) =>
        screen.put(0, 0, e.getMessage)
      case Success(codes) =>
        def result(state: Future[State[SeqIOStream]]) : Future[Seq[Byte]] = {
          bf.exec1(codes, state).flatMap(state => if (state.finished) {
            Future.successful(state.io.out.reverse)
          } else {
            dumpState(config.source, state)
            screen.refresh
            Thread.sleep(config.autoStepMillisec)
            result(Future.successful(state))
          }
        )}
        val res = result(Future.successful(State(BFMachine(), SeqIOStream(Seq.empty, Seq.empty))))
        Await.ready(res, Duration.Inf)      
    }
    screen.put(0, h - 1, "press any key ")
    screen.refresh
    screen.keypress
  }
}
