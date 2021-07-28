package recursion

import scala.annotation.tailrec

object Recursion extends App {

  def fibonacci(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n == 1) prev
      else go(n - 1, curr, prev + curr)
    }
    require(n > 0)
    go(n, 0, 1)
  }

}
