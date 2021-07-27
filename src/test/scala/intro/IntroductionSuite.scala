package intro

import intro.Introduction.factorial
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IntroductionSuite extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {

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


}
