package adhoc

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object JumpingBunnies {

  def main(args: Array[String]): Unit = {
    val _ = readLine()
    val distances = readLine().split(" ").map(BigInt(_))
    println(lcm(distances))
  }

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a else gcd(b, a % b)

  def lcm(a: BigInt, b: BigInt): BigInt =
    a / gcd(a, b) * b

  def lcm(seq: Iterable[BigInt]): BigInt = {
    require(seq.nonEmpty)
    seq.reduce((x, y) => lcm(x, y))
  }
}
