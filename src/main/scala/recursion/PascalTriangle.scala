package recursion

import scala.annotation.tailrec
import scala.io.StdIn

object PascalTriangle {

  def main(args: Array[String]): Unit = {
    triangle(StdIn.readInt()).foreach(row => println(row.mkString(" ")))
  }

  def triangle(n: Int): Vector[Vector[Int]] = {

    @tailrec
    def helper(cnt: Int, acc: Vector[Vector[Int]]): Vector[Vector[Int]] = {
      if (cnt == n) acc
      else helper(cnt + 1, acc :+ row(cnt + 1, acc(cnt - 1)))
    }

    def row(n: Int, prev: Vector[Int]): Vector[Int] = {
      1 +: (1 until n - 1).map(i => prev(i - 1) + prev(i)).toVector :+ 1
    }

    require(n > 0)
    helper(1, Vector(Vector(1)))
  }
}
