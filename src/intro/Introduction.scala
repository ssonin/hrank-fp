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

  def filterPos(numbers: List[Int]): List[Int] = {
    @tailrec
    def go(numbers: List[Int], acc: List[Int], keepHead: Boolean): List[Int] = {
      if (numbers.isEmpty) acc
      else {
        if (keepHead) go(numbers.tail, numbers.head :: acc, false)
        else go(numbers.tail, acc, true)
      }
    }

    go(numbers, List.empty, true)
  }

  def fill(n: Int): List[Int] = {
    @tailrec
    def go(n: Int, acc: List[Int]): List[Int] = {
      if (n < 0) acc else go(n - 1, n :: acc)
    }

    go(n - 1, List.empty)
  }

  def abs(numbers: List[Int]): List[Int] = numbers map {
    n => if (n < 0) -n else n
  }

//  def readInt: Int = {
//    scala.io.StdIn.readInt
//  }

//  val input = Source.stdin.getLines().toList.map(Integer.parseInt)
//  val nTimes = input.head
//  val numbers = input.tail
//
//  println(f(nTimes, numbers))

  println(filterList(3, List(10, 9, 8, 2, 7, 5, 1, 3, 0)))
  println(fill(3))
}
