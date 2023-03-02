package lz

import compress.Compressor
import compress.lz.Dictionaries.empty

import scala.annotation.tailrec
/** The LZ78 compression method */
object LZ78 extends Compressor[Char, Seq[(Int, Char)]] {
  /** @inheritdoc */
  def compress(msg: Seq[Char]): Seq[(Int, Char)] = {
    make_Lz78(msg)
  }
  @tailrec
  def make_Lz78(a_string: Seq[Char], counter: Int = 0,
                end: Int = 1, dicProviso: IndexedSeq[String] = empty,
                aTuple: Seq[(Int, Char)] = Seq[(Int, Char)]()): Seq[(Int, Char)] = {
    /**  @param :
     * a_string  : list given by the user
     * end  : number of the end of index while finding the item
     * aTuple : list the output (counter, char)
     * counter : number index position of char in aTuple
     * dicProviso : list of string to keep tracking aTuple
     @return : return a par aTuple as a sequence of tuple
    */
    if (a_string.size <= 0) { // stop when list is empty
      return aTuple // return the final list
    }
    val item = a_string.slice(0, end).mkString("") // get item and make it a list
    if (!dicProviso.contains(item)) { // if an item is not in the dict then add to dict
      val lastItem = item.dropRight(1)
      // check if the item, (except the end) is in the dict provisoire
      // if yes add the number of the index of the same item found in dict and the last item
      // end slice list from (endItem to end )
      val newCounter = if (dicProviso.contains(lastItem)) dicProviso.indexOf(lastItem) else 0
      make_Lz78(a_string.drop(end), counter = 0, end = 1,
        dicProviso :+ item, aTuple :+ (newCounter, item.last))
    }
    else {
      // if last item is already in the dicProviso
      if (end > a_string.size && dicProviso.contains(item)) {
        make_Lz78(a_string = Seq(), counter = dicProviso.indexOf(item), end,
          dicProviso, aTuple :+ (dicProviso.indexOf(item), '/'))
      } else // otherwise increase counter and the end of dict
        make_Lz78(a_string, counter + 1, end + 1, dicProviso, aTuple)
    }
  }
  /** @inheritdoc */
  def uncompress(res: Seq[(Int, Char)]): Option[Seq[Char]] = {
    un_Lz78(res)
  }
  @tailrec
  def un_Lz78(list_conv: Seq[(Int, Char)],
              a_string: Seq[Char] = Seq(), dicProviso: IndexedSeq[String] = empty
             ): Option[Seq[Char]] = {
    /** @param :
     * list_conv : list given by the user
     * a_string : list the output
     * dicProviso : list of string to keep tracking aTuple
     * @return : return a par aTuple as a sequence of tuple
     */
    if (list_conv.size <= 0) {// if list is empty stop
      if (a_string.isEmpty)  Option(a_string) // if the input list is empty
      else if (  a_string.last == '/' ) { // if a "/" is there.(handeling the last item repeated before check line 36)
         Option(a_string.dropRight(1)) // drop last last value "/"
      } else{
        Option(a_string) // return the last item
    }}
    else {
      val itemOut = list_conv.head // first item from the list
      if (dicProviso.contains(itemOut._2.toString)& itemOut._1 ==0) { // if the item is in the list and it's counter index is 0
        // then drop first item of list_conv and and add char to the s_string and add to dicProviso that char
        un_Lz78(list_conv.drop(1), a_string :+ itemOut._2, dicProviso :+ itemOut._2.toString)
      }
      else { // otherwise find the char from dicProviso by indexing and add char from list_conv
        val value = dicProviso(itemOut._1) + itemOut._2.toString
        // and do the same by removing the first item and adding to the a_string and append new chars to dicProviso
        un_Lz78(list_conv.drop(1), a_string ++ value.toCharArray, dicProviso :+ value)
      }
    }
  }
}