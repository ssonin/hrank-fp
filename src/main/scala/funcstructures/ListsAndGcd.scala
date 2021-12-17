package funcstructures

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.io.StdIn.readLine

object ListsAndGcd {

  def main(args: Array[String]): Unit = {
    val primeProducts = readInput()
    val result = primeProducts.reduce((x, y) => gcd(x, y))
    println(result.foldLeft(new StringBuilder()){case (acc, (k, v)) => acc.append(s"$k $v ")})
  }

  def readInput(): IndexedSeq[Map[Int, Int]] =
    (1 to readLine().toInt)
      .map(_ => readLine.split(" ").map(_.toInt)).map(arr => pairs(arr).toMap)

  def pairs(arr: IndexedSeq[Int]): IndexedSeq[(Int, Int)] =
    for (i <- arr.indices.init by 2) yield (arr(i), arr(i + 1))

  def gcd(x: Map[Int, Int], y: Map[Int, Int]): Map[Int, Int] = {
    @tailrec
    def helper(x: Map[Int, Int], y: Map[Int, Int], acc: Map[Int, Int]): Map[Int, Int] = {
      if (x.isEmpty) acc
      else {
        val (k, v) = x.head
        if (y.contains(k)) helper(x.tail, y, acc + (k -> math.min(v, y(k))))
        else helper(x.tail, y, acc)
      }
    }
    helper(x, y, TreeMap())
  }
}
