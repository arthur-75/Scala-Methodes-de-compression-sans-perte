package MethodesStatistic



import MethodesStatistic.compression.CompressedData

import java.nio.charset.StandardCharsets



object testShannonFano extends App  {
  println("ShanonFano test : ")

  implicit val logger: Logger = Logger.getConsoleLogger(Logger.DEBUG)

  val compressor = ShannonFano()

  val textToCompress: String = "ACABADADEAABBAAAEDCACDEAAABCDBBEDCBACAE"
  println(f"The sentence is : ${textToCompress}")

  val bytes: List[Byte] = textToCompress.getBytes(StandardCharsets.US_ASCII).toList

  val cData: CompressedData = compressor.compress(bytes)

  val decodedBytes = compressor.decompress(cData)

  val decompressedString = new java.lang.String(decodedBytes.toArray, "us-ascii") //decodedBytes.map(_.toChar).mkString("[", "", "]")


  println(s"Decompressed content matched with original: ${decompressedString}")

}

