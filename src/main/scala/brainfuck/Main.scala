import net.team2xh.scurses._
import scala.collection.immutable._
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

    def dumpState(source: String, state :State): Unit = {
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

    bf.parse(config.source).flatMap(codes => {
      val init : Try[State] = Success(State(BFMachine(), IO(Seq.empty, Seq.empty)))
      dumpState(config.source, init.toOption.get)
      screen.refresh()
      Iterator.continually(0)
      .scanLeft(init)((se, d) => se.flatMap(state => {
        val newState = bf.exec1(codes, state)
        newState.foreach(state => {
          dumpState(config.source, state)
          screen.refresh()
        })
        Thread.sleep(10)
        newState
      }))
      .dropWhile(res => res.isSuccess && !res.get.finished)
      .take(1).toSeq.head
    })
    screen.put(0, h - 1, "press any key ")
    screen.refresh
    screen.keypress
  }
}
