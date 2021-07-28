package recursion

import scala.io.StdIn
import scala.util.matching.Regex

object StringCompression {

  val delim: Regex = raw"(?<=(.))(?!\1)".r

  def main(args: Array[String]): Unit = {
    println(compress(StdIn.readLine()))
  }

  def compress(s: String): String = {
    delim.split(s).map(t => encode(t)).mkString
  }

  def encode(token: String): String = token match {
    case s if s.length < 2 => s
    case s => s.head + s.length.toString
  }
}
