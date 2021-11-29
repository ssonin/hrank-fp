package adhoc

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object RotateString extends App {

  val strings = (0 until readLine.toInt).map(_ => readLine)
  val result = strings.map(rotate).map(v => v.mkString(" ")).mkString("\n")
  println(result)

  def rotate(s: String): Vector[String] = {
    @tailrec
    def helper(sb: StringBuilder, n: Int, acc: Vector[String]): Vector[String] = {
      if (sb.length == 1) Vector(sb.toString)
      else if (n == 0) acc
      else {
        val rotated = sb.tail append sb.head
        helper(rotated, n - 1, acc :+ rotated.toString)
      }
    }

    helper(new StringBuilder(s), s.length, Vector())
  }
}
