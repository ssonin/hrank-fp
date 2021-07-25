package intro

import scala.annotation.tailrec
import scala.math.pow

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
        if (keepHead) go(numbers.tail, numbers.head :: acc, !keepHead)
        else go(numbers.tail, acc, !keepHead)
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

  def sumOfOddNumbers(numbers: List[Int]): Int = {
    numbers.filter(_ % 2 != 0).sum
  }

  /**
   * Returns Euler's number e raised to the power of a Double value.
   * Uses the first 10 terms of the series expansion:
   * 1 + x + `x^2^`/2! + `x^3^`/3! + ...
   * @param x the exponent to raise e to.
   * @return the value `e^a^`, where e is the base of the natural logarithms.
   */
  def eToX(x: Double): Double = {

    @tailrec
    def go(x: Double, term: Int, acc: Double): Double = {
      if (term == 0) acc + 1
      else go(x, term - 1, pow(x, term) / factorial(term) + acc)
    }

    def factorial(n: Int): Int = {
      @tailrec
      def go(n: Int, acc: Int): Int = {
        if (n < 2) acc
        else go(n - 1, n * acc)
      }
      require(n >= 0)
      go(n, 1)
    }

    go(x, 9, 0)
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
  println(eToX(1))
  println(eToX(2))
  println(eToX(0.5))
  println(eToX(5.0000))
  println(eToX(20.0000))
}
