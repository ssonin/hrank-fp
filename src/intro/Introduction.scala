package intro

import scala.annotation.tailrec

object Introduction extends App {

  def replicate(nTimes: Int, numbers: List[Int]): List[Int] = {
    @tailrec
    def go(nTimes: Int, acc: List[Int], number: Int): List[Int] =
      if (nTimes == 0) acc else go(nTimes - 1, number :: acc, number)

    numbers.flatMap(go(nTimes, List.empty, _))
  }

  def filterList(delim: Int, numbers: List[Int]): List[Int] = {
    @tailrec
    def go(predicate: Int => Boolean, acc: List[Int], numbers: List[Int]): List[Int] = {
      if (numbers.isEmpty) acc
      else if (predicate(numbers.head)) go(predicate, numbers.head :: acc, numbers.tail)
      else go(predicate, acc, numbers.tail)
    }

    go(x => x < delim, List.empty, numbers).reverse
  }

//  val input = Source.stdin.getLines().toList.map(Integer.parseInt)
//  val nTimes = input.head
//  val numbers = input.tail
//
//  println(f(nTimes, numbers))

  println(filterList(3, List(10, 9, 8, 2, 7, 5, 1, 3, 0)))

}
