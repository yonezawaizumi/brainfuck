import scala.collection.immutable._

// NOTE: エラー
case class Error(val message: String, val pos: Int = 0, val ptr: Int = 0)

// NOTTE: 実行コード1つ
case class Code(val c: Char, val close: Option[Int] = None, val open: Option[Int] = None)

// NOTE: [] をパースするための状態
case class Loops(val open: Seq[Int] = Seq.empty, val open2Close :Map[Int, Int] = Map.empty, val close2Open: Map[Int, Int] = Map.empty) {
  def isEmpty = open.isEmpty
  def head = open.head
  def push(pos: Int) = Loops(pos +: open, open2Close, close2Open)
  def pop(pos: Int) : Either[Error, Loops] = if (isEmpty) {
    Left(Error("loop underflow", pos))
  } else {
    Right(Loops(open.tail, open2Close + (open.head -> pos), close2Open + (pos -> open.head)))
  }
}

// NOTE: BF に操作される機械
case class BFMachine(val heap: Vector[Byte] = Vector(0), val pos: Int = 0, val ptr: Int = 0) {
  def > = if (ptr < heap.length - 1) {
    BFMachine(heap, pos + 1, ptr + 1)
  } else {
    BFMachine(heap :+ 0, pos + 1, ptr + 1)
  }
  def < : Either[Error, BFMachine] = if (ptr > 0) {
    Right(BFMachine(heap, pos + 1, ptr - 1))
  } else {
    Left(Error("pointer underrun", pos, ptr))
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

case class IO(val in: Seq[Byte], val out: Seq[Byte] = Seq.empty) {
  def get = if (in.isEmpty) (this, None) else (IO(in.tail, out), Some(in.head))
  def put(c: Byte) = IO(in, c +: out)
}

case class State(machine: BFMachine, io: IO, finished: Boolean = false) {
  // I/O がない限りはメモリーのみ更新
  def updated(machine: BFMachine) : Either[Error, State] = Right(State(machine, io))
  def finish = State(machine, io, true)
}

class Brainfuck {
  // NOTE: ソースコードをスキャンして [ と ] の移動位置を記憶
  def parse(code: String) : Either[Error, Vector[Code]] = {
    code.zipWithIndex.foldLeft(Right(Loops()): Either[Error, Loops])((loops, c) => loops.flatMap(l => c._1 match {
      // NOTE: [ の位置をスタックに積む
      case '[' => Right(l.push(c._2))
      // NOTE: ] が出たらスタックトップの位置と自分の位置を双方向リンクとして保存
      case ']' => l.pop(c._2)
      case _ => Right(l)
    })).flatMap(loops => if (loops.isEmpty) {
      Right(code.zipWithIndex.map(c => Code(c._1, loops.open2Close.get(c._2), loops.close2Open.get(c._2))).toVector)
    } else {
      Left(Error("loop overflow", loops.head))
    })
  }
  // NOOTE: コード1つを実行
  def exec1(codes: Vector[Code], state: State) : Either[Error, State] = {
    val code = codes(state.machine.pos)
    // NOTE: 1命令ずつ実行する
    val newState = code.c match {
      case '>' => state.updated(state.machine.>)
      case '<' => state.machine.<.map(machine => State(machine, state.io))
      case '+' => state.updated(state.machine.+)
      case '-' => state.updated(state.machine.-)
      case '.' =>
        val (machine, c) = state.machine.dot
        Right(State(machine, state.io.put(c)))
      case ',' =>
        val res = state.io.get
        //println(res)
        Right(res._2 match {
          case None => state.finish
          case Some(c) => State(state.machine.comma(c), res._1)
        })
      // NOTE: codel.close や code.open には Loops で解析した値が必ずある
      case '[' => state.updated(state.machine.open(code.close.get))
      case ']' => state.updated(state.machine.close(code.open.get))
      case _ => state.updated(state.machine.noop)
    }
    if (newState.isLeft || codes.lengthCompare(newState.toOption.get.machine.pos) > 0) {
      newState
    } else {
      newState.map(_.finish)
    }
  }

  // NOTE: パース済みコードを実行
  def exec(codes: Vector[Code], gets: Seq[Byte]) : Either[Error, Seq[Byte]] = {
    val init : Either[Error, State] = Right(State(BFMachine(), IO(gets)))
    // NOTE: 無限ループ
    Iterator.continually(0)
    // NOTE: 左畳み込みをしつつ途中経過をぜんぶ吐き出す
    .scanLeft(init)((se, d) => se.flatMap(state => exec1(codes, state)))
    // NOTE: 実行中の途中経過は捨てる
    .dropWhile(res => res.isRight && !res.toOption.get.finished)
    // NOTE: 実行終了 or エラー時の先頭以外を捨てて実態を取り出す
    .take(1).toSeq.head
    // NOTE: 返したい IO.out は Seq なので出力と逆順のコレクションになっている
    .map(state => state.io.out.reverse)
  }

  def run(code: String, gets: Seq[Byte]) : Either[Error, Seq[Byte]] = {
    parse(code).flatMap(codes => exec(codes, gets))
  }
}
