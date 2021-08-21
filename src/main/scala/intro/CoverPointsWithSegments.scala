package intro

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.Random

/**
 * https://stepik.org/lesson/13249/step/6?unit=3434
 *
 * Exceeds time limit in Test 4.
 */
object CoverPointsWithSegments {

  def main(args: Array[String]): Unit = {
    val (numSegments, _) = readIntTuple
    val (leftEnds, rightEnds) = (new ArrayBuffer[Int](numSegments), new ArrayBuffer[Int](numSegments))
    for (_ <- 0 until numSegments) {
      val (left, right) = readIntTuple
      leftEnds += left
      rightEnds += right
    }
    val points = StdIn.readLine().split(" ").map(_.toInt)

    val result = points.map(p => count(sort(leftEnds), p) - count(sort(rightEnds), p))
    println(result.mkString(" "))
  }


  def count(points: ArrayBuffer[Int], target: Int): Int = {
    @tailrec
    def search(start: Int, end: Int): Int = {
      if (points(start) > target) start
      else if (points(end) <= target) end + 1
      else {
        val mid = (start + end) / 2
        if (points(mid) <= target) search(mid + 1 , end)
        else search(start, mid)
      }
    }

    search(0, points.length - 1)
  }

  def sort(arr: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    def quicksort3way(arr: ArrayBuffer[Int], lo: Int, hi: Int): ArrayBuffer[Int] = {
      if (hi <= lo) return arr
      var lt = lo; var i = lo + 1; var gt = hi
      val pivot = arr(lo)
      while (i <= gt) {
        if (arr(i) < pivot) {
          swap(arr, i, lt)
          i += 1; lt += 1
        } else if (arr(i) > pivot) {
          swap(arr, i, gt)
          gt -= 1
        } else i += 1
      }
      quicksort3way(arr, lo, lt - 1)
      quicksort3way(arr, gt + 1, hi)
      arr
    }

    def swap(arr: ArrayBuffer[Int], i: Int, j: Int): Unit = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    quicksort3way(Random.shuffle(arr), 0, arr.size - 1)
  }

  def readIntTuple: (Int, Int) = {
    StdIn.readf2("{0} {1}") match {
      case (n, m) => (n.toString.toInt, m.toString.toInt)
    }
  }
}
