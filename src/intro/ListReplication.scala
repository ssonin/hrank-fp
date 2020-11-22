package intro

import scala.annotation.tailrec
import scala.io.Source

object ListReplication extends App {

  def f(nTimes: Int, numbers: List[Int]): List[Int] = {
    @tailrec
    def go(nTimes: Int, acc: List[Int], number: Int): List[Int] =
      if (nTimes == 0) acc else go(nTimes - 1, number :: acc, number)

    numbers.flatMap(go(nTimes, List.empty, _))
  }

  val input = Source.stdin.getLines().toList.map(Integer.parseInt)
  val nTimes = input.head
  val numbers = input.tail

  println(f(nTimes, numbers))

}
