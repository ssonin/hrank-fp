package memo

import scala.io.StdIn.readInt

object PentagonalNumbers {

  def main(args: Array[String]): Unit = {
    val numTestCases = readInt()
    val testCases = (0 until numTestCases).map(_ => readInt())
    val result = testCases.map(pentagonal)
    println(result.mkString("\n"))
  }

  def pentagonal(n: Int): BigInt = {
    val bigN = BigInt(n)
    (3 * bigN * bigN - bigN) / 2
  }
}
