package recursion

import scala.io.StdIn
import scala.util.matching.Regex

object StringCompression {

  val delim: Regex = raw"(?<=(.))(?!\1)".r

  def main(args: Array[String]): Unit = {
    println(compress(StdIn.readLine()))
  }

  def compress(s: String): String = {
    delim.split(s).map(encode).mkString
  }

  def encode(token: String): String = {
    if (token.length < 2) token
    else token.head + token.length.toString
  }
}
