package recursion

import scala.annotation.tailrec
import scala.io.StdIn

object StringMingling extends App {

  val s1 = StdIn.readLine()
  val s2 = StdIn.readLine()
  println(mingle(s1, s2))

  def mingle(s1: String, s2: String): String = {
    @tailrec
    def helper(s1: String, s2: String, acc: StringBuilder): String = {
      if (s1.isEmpty && s2.isEmpty) acc.toString
      else helper(s1.tail, s2.tail, acc.append(s1.head).append(s2.head))
    }
    helper(s1, s2, new StringBuilder(s1.length + s2.length))
  }
}
