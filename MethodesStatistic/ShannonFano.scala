package MethodesStatistic

import MethodesStatistic.compression.{Bits, CompressedData, Compressor, Leaf, Node, Tree}

/**
 * This is an implementation of the Shannon-Fano compression encoding scheme.
 *
 */
object ShannonFano {
  def apply()(implicit logger: Logger) = new ShannonFano {
    override def _log(_msg: String, _level: LoggerLevel): Unit = logger._log(_msg, _level)
  }
}

abstract class ShannonFano extends Compressor {
  /**
   * Identify all unique bytes and group them with their associated frequency. An
   * occurrence of a byte with it's frequency (number of times it appears in the input)
   * is represented by Leaf. This function returns a sequence of such Leafs ordered by their
   * descending frequency, i.e. most common Leafs come first.
   * @param bytes
   * @return
   */
  private def makeLeafs(bytes: List[Byte]): List[Leaf] = {
    val groups: List[Leaf] = {
      val x = bytes.groupBy(a => a).map {
        case (key, ls) => Leaf(key, ls.length)
      }.toList.sortBy(_.count)
      x
    }
    groups
  }
  /**
   * Given a sequence of Leaves, this function assembles them in a binary tree according the
   * Shannon-Fano algorithm. This recursively splits the list in half and constructs intermediate nodes.
   * @param ls
   * @return
   */
  protected def makeBinaryTree(ls: List[Leaf]): Tree = {
    if (ls.size > 1) {
      val (left, right) = ls.splitAt(ls.size / 2)
      Node(makeBinaryTree(left), makeBinaryTree(right))
    } else {
      ls.head
    }
  }

  def logEncodingTree(tree: Tree): Unit = {

    def _inner(_tree: Tree, _code: List[Boolean]): Unit = _tree match {
      case Node(left, right) =>
        _inner(left, _code :+ false)
        _inner(right, _code :+ true)
      case leaf@Leaf(_, _) => println(s"${_code.map(b => if (b) "1" else "0").mkString("")}\t= ${leaf}")
    }

    _inner(tree, Nil)
  }

  /**
   * Given the computed binary tree, this function constructs a map where each byte is mapped to a sequence
   * of bits that encode it. The idea is that the number of encoding bits is most of the time less than 8,
   * otherwise the compression would not provide any benefit.
   * @param tree
   * @return
   */
  def encoder(tree: Tree): Map[Byte, Bits] = {

    def _inner(_tree: Tree, _code: List[Boolean], _map: Map[Byte, Bits]): Map[Byte, Bits] = _tree match {
      case Node(left, right) =>
        _inner(left, _code :+ false, _inner(right, _code :+ true, _map))
      case Leaf(value, _) =>
        _map + (value -> _code)
    }

    _inner(tree, Nil, Map.empty)
  }

  override def compress(bytes: List[Byte]): CompressedData = {


    // make the binary encoding tree
    val tree = makeBinaryTree(makeLeafs(bytes))
    val enc = encoder(tree)

    // this type of compression simply emits the appropriate number of bits for each read byte,
    // there is no dictionary matching or anything fancy at all here
    val bits = bytes.flatMap { b: Byte => enc(b) }



    logEncodingTree(tree)
    println("It converts to : ")
    logBits(bits)

    CompressedData(tree, bits)
  }

  override def decompress(cData: CompressedData): List[Byte] = {

    // the _outer loops to the next decompressed byte  while the _inner finds the appropriate leaf given
    // the required number of bits that direct it down the binary tree
    def _outer(_bits: Bits, _bytes: List[Byte]): List[Byte] = {

      // consumes bits until it finds a leaf and returns the byte in it
      def _inner(_bits: Bits, _current: Tree): (Bits, Byte) = {
        _current match {
          case Node(left, _) if !_bits.head =>
            //trace(s".bit(${_bits.size-_bits.size})=0: ${_current}")
            _inner(_bits.tail, left)
          case Node(_, right) if _bits.head =>
            // trace(s".bit(${_bits.size-_bits.size})=1: ${_current}")
            _inner(_bits.tail, right)
          case Leaf(value, _) =>
            (_bits, value)
        }
      }

      if (_bits.isEmpty)
        _bytes
      else {
        val (_b, _byte) = _inner(_bits, cData.tree)
        _outer(_b, _bytes :+ _byte)
      }
    }

    _outer(cData.bits, Nil)
  }

}




