import compress.Compressor

/** The Run-length encoding compression method */
  class RLE[T] extends Compressor[T, Seq[(T, Int)]] {

    /** @inheritdoc */
    def compress(msg: Seq[T]): Seq[(T, Int)] = {
      loopRec(msg: Seq[T], msg.size, 0, msg(0), Seq[(T, Int)]())
    }

    def loopRec(msg: Seq[T], n: Int, counter: Int, last: T, list_ofWord: Seq[(T, Int)]): Seq[(T, Int)] = {
      if (n == 0) {
        return list_ofWord :+ (last, counter)
      }
      val item = msg(msg.size - n)
      if (item == last) {
        loopRec(msg, n - 1, counter + 1, item, list_ofWord)
      }
      else {loopRec(msg, n - 1, 1, item, list_ofWord :+ (last, counter))}
    }
    /** @inheritdoc */
    def uncompress(seq: Seq[(T, Int)]): Option[Seq[T]] = {
      Some(seq.map(x=> List.fill(x._2)(x._1)).flatten)
    }
  }