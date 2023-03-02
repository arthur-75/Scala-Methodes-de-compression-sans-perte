package MethodesStatistic

import scala.annotation.tailrec
import scala.math.log

case object Huffman_com {
  // First if all our tree
  sealed trait EncodingTree[+S]

  // I've decided to make my tree with branches
  case class EncodingBranch[S](left: EncodingTree[S], right: EncodingTree[S]) extends EncodingTree[S]

  //here is my Leaves my leaves are nodes as well
  case class EncodingLeaf[S](value: S) extends EncodingTree[S]

  /** check if a value is in a leaf
   * @param char the character to find in @param tree
   * @return a boolean
   */
  // this function is made to search in all the brunch for a given node(char) in the tree
  def has[S](tree: EncodingTree[S], char: S): Boolean = tree match {
    //First option is it in the leaf ?
    case EncodingLeaf(node) => if (node == char) true
    else false
    //Seconde option is it in on the right or the left branches ?
    // this recursive function will visit all the tree and will need at least on true
    case EncodingBranch(left, right) => has(left, char) || has(right, char)
  }

  /** create the tree
   * @param tex the list containing the tree and the number of characters of each type
   * @return the tree with the two 'smallest' characters
   */
  // here is the tree we decided to build from the bottom to the top
  @tailrec
  def treeHuff[S](tex: List[(EncodingTree[S], Int)]): List[(EncodingTree[S], Int)] = {
    if (tex.length == 1) tex // keep the last one left as the main node
    else {
      val left = tex(0) // taking the smallest char
      val right = tex(1) // taking the one before the smallest char
      // building the Branches for tow chars and adding the sum of thier  frequancies
      val together = (EncodingBranch(left._1, right._1), left._2 + right._2)
      // droping the smallest two chars and re sorting by the frequences
      treeHuff((together :: tex.drop(2)).sortBy(_._2))
    }
  }
  /** calculate the entropy
   * @param source is the string we want to know the entropy
   * @return entropy of the message
   */
  def entropy[S](source: Seq[S] ) : Double = {
    // calcule entropy
    val occurrences:List[(S, Int)]= freq(source) // get the occurance
    val len: Int = source.length //length of the messeage
    val log2occ: Iterable[Double] = occurrences.map(x => (x._2.toDouble / len) * (log(x._2) - log(len)) / log(2))
    -1 * log2occ.sum // entropy formule
  }

  /** add the branches to the tree
   * @param tree the existing tree
   * @param char the character to focus on
   * @param Phrase the string to stock the bits sequences
   * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in enconding tree
   */
  @tailrec
  def goPath[S](tree: EncodingTree[S], char: S, Phrase: String = ""): String = tree match {
    // Building Bits path of the tree
    case EncodingLeaf(_) => Phrase // if it is the end then return the word
    // check left and right branch if it has the letter than add 0 to left and 1 to the right
    case EncodingBranch(left, right) => if (has(left, char)) goPath(left, char, Phrase + '0') else goPath(right, char, Phrase + '1')
  }

  /**
   * @param text
   * @return ordered RLE
   */
  def freq[S](text:Seq[S]) : List[(S,Int)] = {
    // the frequcnies well ordered
    text.groupBy(chars => chars).map(group => (group._1, group._2.length)).toList.sortBy(_._2)
  }

  def uncompress(tex1:String,tex2:String) ={
    val frequencies= freq(tex1)
    val Leaves = frequencies.map(x => (EncodingLeaf(x._1), x._2))
     tex2
  }

  /** Computes the next value corresponding to the beginning of bit sequence (if possible)
   * @param text the sequence
   * @return a list of the frequences of the leaves
   */
  def getLeavesFreq[S](text: Seq[S]): List[String] = {
    val frequencies= freq(text)
    val Leaves = frequencies.map(x => (EncodingLeaf(x._1), x._2))
    val huffmanTree = treeHuff(Leaves).head._1
    Leaves.map(x => goPath(huffmanTree, x._1.value)).toSeq

  }
  def theRtr(text1:String)={ text1}

  /**
    @param bits:  text en code Bitq
   @param text : le text d'entrée
    @param freq1 : la table d'occurance
    @param counter : la valeure de retouner pour '(Longueur moyenne du code)
    @param lengthText : longeur de text   * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in enconding tree
   */
  @tailrec
  def length_avarage[S](bits: Seq[String], text:Seq[S], freq1: List[(S, Int)]=Nil, counter: Double=0
                        ,lengthText:Int=0): Double = {


    if (bits.length<1) counter // return avarage
    else if (freq1==Nil) length_avarage(bits,Nil,freq(text),counter,text.length) //get occurances
    else {
      val len= bits.head.length.toDouble
      val len1=freq1.head._2.toDouble


      length_avarage(bits.drop(1),Nil,freq1.drop(1),counter + ( len*len1/lengthText ),lengthText)
    }
  }

  /**
   * @param msg is the sequence we want to compress
   * @return the sequence of the sequences of the leaves
   */
  def compress[S](msg: Seq[S]): Seq[String] = {
    if (msg.isEmpty) Nil
    else getLeavesFreq(msg)
  }



  def main(args: Array[String]): Unit = {

    //Test
    val text = "this is an example for huffman encoding"
    // transform the text into a list of tuples.
    // each tuple contains a Leaf node containing a unique character and an Int representing that character's weight
    val frequencies = text.groupBy(chars => chars).map(group => (group._1, group._2.length)).toList.sortBy(_._2)
    val Leaves = frequencies.map(x => (EncodingLeaf(x._1), x._2))
    val ReturnTree= theRtr(text)
    //val huffmanTree = merge(frequencies).head._1
    // output the resulting character codes
    // the tree
    val huffmanTree = treeHuff(Leaves).head._1
    println("Char\t\t\t\tWeight\tCode")
    Leaves.foreach(x => println(x._1 + "\t\t" + x._2 + s"/${text.length}" + s"\t${goPath(huffmanTree, x._1.value)}"))
  println("")
    println(s"The entropy is : ${entropy(text)}")
    val the_com=compress(text)
  println(s"The list of bites for Huffman is ${the_com}")
    println(s"length_avarage ${length_avarage(bits= the_com,text)}")
    println(s"uncompress is : ${uncompress(text,ReturnTree)}")
    println("")
    println("Thanks for you. :) ")
    println("")

  }






  // Building the tree


}
