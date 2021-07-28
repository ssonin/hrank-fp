package recursion

import intro.Introduction.factorial
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import recursion.StringCompression.compress

class RecursionSuite extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {

  property("Factorial evaluation") {
    val input = Table(
      ("n", "expected"),
      (0, BigInt(1)),
      (1, BigInt(1)),
      (2, BigInt(2)),
      (3, BigInt(6)),
      (4, BigInt(24)),
      (5, BigInt(120)),
      (17, BigInt("355687428096000"))
    )
    forAll (input) { (n: Int, expected: BigInt) =>
      factorial(n) should equal (expected)
    }
  }

  property("String compression") {
    val input = Table(
      ("s", "expected"),
      ("abcaaabbb", "abca3b3"),
      ("abcd", "abcd"),
      ("aaabaaaaccaaaaba", "a3ba4c2a4ba"),
      ("", "")
    )
    forAll (input) { (s: String, expected: String) =>
      compress(s) should equal (expected)
    }
  }

}
