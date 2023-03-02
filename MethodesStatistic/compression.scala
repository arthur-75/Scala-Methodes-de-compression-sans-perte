package MethodesStatistic

package object compression {
  type Bit = Boolean
  type Bits = List[Bit]

  /**
   * The binary tree used to represent the encoder and decoder
   */
  sealed trait Tree
  case class Leaf(value: Byte, count: Int) extends Tree {
    override def toString: String = f"Leaf('${value.toChar}'=0x$value%02X,$count)"
  }
  case class Node(left: Tree, right: Tree) extends Tree
  /**
   * Represents the compressed data in memory
   * @param tree the encoding amd decoding binary tree used to compress/uncompress data
   * @param bits the bit sequence representing the compressed data
   */
  case class CompressedData(tree: Tree, bits: Bits)
  /**
   * Defines the basic Compress behaviour
   */
  trait Compressor extends Logger {

    /**
     * Encode, i.e. compress the passed byte sequence
     * @param bytes
     * @return
     */
    def compress(bytes: List[Byte]): CompressedData

    /**
     * Decode, i.e. uncompress the passed CompressedData to the original byte sequence
     * @param data
     * @return
     */
    def decompress(data: CompressedData): List[Byte]

    /**
     * Logs the passed bit stream as ones and zeroes as they appear in the bit stream.
     * No byte grouping or "endianness" is applied.
     * @param bits
     */
    protected def logBits(bits: Bits): Unit = {
      // dump 8 bytes of binary per line to the console


      println(bits.map(b => if (b) '1' else '0')
        .grouped(8).map(_.mkString(""))
        .grouped(8).map(_.mkString(" "))
        .mkString("\n"))

    }

  }
}
