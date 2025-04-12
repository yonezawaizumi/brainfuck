import scala.collection.immutable._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util._

// NOTE: エラー
// TODO: すでにコード実行中の場合は機械の状態もダンプすべき
case class Error(message: String, pos: Int = 0, ptr: Int = 0) extends java.lang.Exception(message)

// NOTTE: 実行コード1つ
case class Code(c: Char, close: Option[Int] = None, open: Option[Int] = None)

// NOTE: [] をパースするための状態
case class Loops(open: Seq[Int] = Seq.empty, open2Close :Map[Int, Int] = Map.empty, close2Open: Map[Int, Int] = Map.empty) {
  def isEmpty = open.isEmpty
  def head = open.head
  def push(pos: Int) = Loops(pos +: open, open2Close, close2Open)
  def pop(pos: Int) : Try[Loops] = if (isEmpty) {
    Failure(Error("loop underflow", pos))
  } else {
    Success(Loops(open.tail, open2Close + (open.head -> pos), close2Open + (pos -> open.head)))
  }
}

// NOTE: BF に操作される機械
case class BFMachine(heap: Vector[Byte] = Vector(0), pos: Int = 0, ptr: Int = 0) {
  def > = if (ptr < heap.length - 1) {
    BFMachine(heap, pos + 1, ptr + 1)
  } else {
    BFMachine(heap :+ 0.toByte, pos + 1, ptr + 1)
  }
  def < : Try[BFMachine] = if (ptr > 0) {
    Success(BFMachine(heap, pos + 1, ptr - 1))
  } else {
    Failure(Error("pointer underrun", pos, ptr))
  }
  def + = {
    val c = heap(ptr) + 1
    BFMachine(heap.updated(ptr, if (c > Byte.MaxValue) Byte.MinValue else c.toByte), pos + 1, ptr)
  }
  def - = {
    val c = heap(ptr) - 1
    BFMachine(heap.updated(ptr, if (c < Byte.MinValue) Byte.MaxValue else c.toByte), pos + 1, ptr)
  }
  def dot : (BFMachine, Byte) = (BFMachine(heap, pos + 1, ptr), heap(ptr))
  def comma(c: Byte) = BFMachine(heap.updated(ptr, c), pos + 1, ptr)
  def open(close: Int) = BFMachine(heap, (if (heap(ptr) == 0) close else pos) + 1, ptr)
  def close(open: Int) = BFMachine(heap, (if (heap(ptr) != 0) open else pos) + 1, ptr)
  def noop = BFMachine(heap, pos + 1, ptr)
  override def toString = "pos: %d, ptr: %d".format(pos, ptr) + heap.map(b => {
    " %02x".format(if (b < 0) 256 + b else b.toInt)
  }).mkString("")
}

// NOTE: I/O
trait IOStream[S] {
  def get : Future[(S, Option[Byte])]
  def put(b: Byte) : Future[S]
}

// NOTE: Seq で動機的な I/O
case class SeqIOStream(in :Seq[Byte], out :Seq[Byte]) extends IOStream[SeqIOStream] {
  def get : Future[(SeqIOStream, Option[Byte])] = {
    Promise[(SeqIOStream, Option[Byte])].success(
      if (in.isEmpty) {
        (this, None)
      } else {
        (SeqIOStream(in.tail, out), Some(in.head))
      }
    ).future
  }
  def put(b: Byte) : Future[SeqIOStream] = Future.successful(SeqIOStream(in, b +: out))
}

// NOTE: コード1つごとの機械の状態
// FIXME: finished の判定が Future 絡みで怪しそう（再帰で finished を立てても止まらないことがある）
case class State[S <: IOStream[S]](machine: BFMachine, io: S, step: Int = 1, finished: Boolean = false) {
  // I/O がない限りはメモリーのみ更新
  def updated(machine: BFMachine) : Future[State[S]] = Future.successful(State(machine, io, step + 1))
  def ioUpdated(io: Future[S]) : Future[State[S]] = io.map(io => State[S](machine, io, step))
  def finish = State(machine, io, step, true)
}

class Brainfuck {
  // NOTE: ソースコードをスキャンして [ と ] の移動位置を記憶
  def parse(code: String) : Try[Vector[Code]] = {
    code.zipWithIndex.foldLeft(Success(Loops()): Try[Loops])((loops, c) => loops.flatMap(l => c._1 match {
      // NOTE: [ の位置をスタックに積む
      case '[' => Success(l.push(c._2))
      // NOTE: ] が出たらスタックトップの位置と自分の位置を双方向リンクとして保存
      case ']' => l.pop(c._2)
      case _ => Success(l)
    })).flatMap(loops => if (loops.isEmpty) {
      Success(code.zipWithIndex.map(c => Code(c._1, loops.open2Close.get(c._2), loops.close2Open.get(c._2))).toVector)
    } else {
      Failure(Error("loop overflow", loops.head))
    })
  }
  // NOTE: コード1つを実行
  def exec1[S <: IOStream[S]](codes: Vector[Code], state: Future[State[S]]) : Future[State[S]] = state.flatMap(state => {
    val code = codes(state.machine.pos)
    // NOTE: 1命令ずつ実行する
    (code.c match {
      case '>' => state.updated(state.machine.>)
      case '<' => Promise[State[S]].complete(state.machine.<.map(machine => State(machine, state.io, state.step + 1))).future
      case '+' => state.updated(state.machine.+)
      case '-' => state.updated(state.machine.-)
      case '.' =>
        val (machine, c) = state.machine.dot
        state.ioUpdated(state.io.put(c)).map(state => State[S](machine, state.io, state.step + 1))
      case ',' =>
        state.io.get.map(res => {
          //println(res)
          res._2 match {
            case None => state.finish
            case Some(c) => State[S](state.machine.comma(c), res._1, state.step + 1)
          }
        })
      // NOTE: codel.close や code.open には Loops で解析した値が必ずある
      case '[' => state.updated(state.machine.open(code.close.get))
      case ']' => state.updated(state.machine.close(code.open.get))
      case _ => state.updated(state.machine.noop)
    }).map(state => if (codes.lengthCompare(state.machine.pos) > 0) {
      state
    } else {
      state.finish
    })
  })

  // NOTE: パース済みコードを実行
  def exec(codes: Vector[Code], gets: Seq[Byte]) : Future[Seq[Byte]] = {
    // NOTE: 結果が出るまで末尾再帰風で畳み込む（末尾再帰ではない）
    def result(state: Future[State[SeqIOStream]]) : Future[Seq[Byte]] = {
      exec1(codes, state).flatMap(state => if (state.finished) {
        Future.successful(state.io.out.reverse)
      } else {
        result(Future.successful(state))
      }
    )}
    result(Future.successful(State(BFMachine(), SeqIOStream(gets, Seq.empty))))
  }

  def run(code: String, gets: Seq[Byte], result: Try[Seq[Byte]] => Unit) : Unit = {
    Future.fromTry(parse(code)).flatMap(codes => exec(codes, gets)).onComplete(result)
  }
}
