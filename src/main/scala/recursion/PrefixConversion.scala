package recursion

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object PrefixConversion {

  def main(args: Array[String]): Unit = {
    val (x, y) = (readLine, readLine)
    val (prefix, x_, y_) = compress(x, y)
    println(List(
      s"${prefix.length} $prefix",
      s"${x_.length} $x_",
      s"${y_.length} $y_").mkString("\n"))
  }

  def compress(x: String, y: String): (String, String, String) = {
    @tailrec
    def helper(x: String, y: String, acc: StringBuilder): (String, String, String) = {
      if (x.isEmpty || y.isEmpty || x.head != y.head) (acc.toString(), x, y)
      else helper(x.tail, y.tail, acc append x.head)
    }
    helper(x, y, new StringBuilder)
  }
}
