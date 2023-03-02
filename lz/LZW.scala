package lz

import compress.Compressor
import compress.lz.Dictionaries.{ASCII, Dictionary}

/** The LZW compression method
  * @param initialDictionary the starting dictionary
  */
class LZW(val initialDictionary : Dictionary = ASCII) extends Compressor[Char, Seq[Int]]
{
    type Dictionary = IndexedSeq[String]

    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[Int] = { LZW(msg)}

    def LZW(a_string: Seq[Char],end: Int = 1, list_word: Seq[Int] = Seq[Int](),
            ASCII : IndexedSeq[String] = (0 to 255) map (_.toChar.toString)): Seq[Int] = {

      /** @param :
       *  a_string : list given by the user
       *  end : number of the end of index while finding the item
       *  list_word : list the output (counter, char)
       *  ASCII : list of all chars from 0 to 255
       *  @return : return the the result of compression
       *  */
          if (a_string.size <= 0) { // stop when list is empty
            list_word // return the final list
          }
          else{
            if (a_string.size <= end){ // last tour
              // add the last item found and make s_string empty
              LZW(a_string.empty,end,
                list_word :+ ASCII.indexOf(a_string.mkString))
            }else{ // p for previous et c for current
              val P=a_string.slice(0,end).mkString
              val C=a_string(end).toString
              if  (!ASCII.contains(P+C)) { // P+C does not exist in ASCII
                // then add to the dic ASCII and drop the first item from list a_string  and initial end
                // and add the p char to list_word
                LZW(a_string.drop(end),end=1,list_word :+ ASCII.indexOf(P.mkString),ASCII :+ P+C)
              }else { // otherwise increase end by 1
                LZW(a_string,end+1,list_word,ASCII)
              }
            }
          }
        }
    /** @inheritdoc */
    def uncompress(res : Seq[Int]) : Option[Seq[Char]]={
      Some(stringToSeq(LZW_Un(res)))
    }

    def stringToSeq (Chaine : String) : Seq[Char] = {
      val seq : Seq[Char] = Chaine
      return seq
    }

    def LZW_Un(list_word: Seq[Int],msg: String="",end: Int = 1,
               ASCII : IndexedSeq[String] = initialDictionary): String =
    {
      /** @param :
       * msg : output
       * end(int) : number of the end of index while finding the item
       * list_word(int, char) : compress's output  (counter, char)
       * ASCII(string): list of all chars from 0 to 255
       * @return : return the the result of compression
       * */

      if (list_word.size <= 0) // stop when list is empty
      {
        msg // return the finale msg uncompress
      }
      else
      { // p for previous et c for current
        val P = for (i <- list_word.slice(0,end)) yield (ASCII(i))
        if (list_word.size <= end) // last tour
        {
          // add the last item found and make list_word empty
          LZW_Un(list_word.empty,msg+P.mkString,end,ASCII)
        }
        else
        {
          val C=ASCII(list_word(end))
          if (!ASCII.contains(P.mkString+C))
          { // P+C does not exist in ASCII
            // then add to the dic ASCII and drop the first item from list_word and initial end
            // and add the p char to msg
            LZW_Un(list_word.drop(end),msg+P.mkString,end=1,ASCII:+P.mkString+C)
          }
          else
          { // otherwise increase end by 1
            LZW_Un(list_word,msg,end+1,ASCII)
          }
        }
      }
    }
}


