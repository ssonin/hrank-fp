package adhoc

import adhoc.JumpingBunnies.{gcd, lcm}
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AdhocSuite extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers{

  property("Greatest common divisor") {
    val input = Table(
      ("a", "b", "expected"),
      (0, 42, 42),
      (42, 0, 42),
      (3, 5, 1),
      (48, 18, 6),
      (18, 48, 6)
    )

    forAll(input) { (a: Int, b: Int, expected: Int) =>
      gcd(a, b) should equal (expected)
    }
  }

  property("Least common multiple") {
    val input = Table(
      ("numbers", "expected"),
      (List(2, 3, 4), BigInt(12)),
      (List(1, 3), BigInt(3)),
      (List(83, 75, 60, 37, 10, 26, 98, 15, 13, 22), BigInt("6455549100"))
    )

    forAll(input) {(numbers: Iterable[Int], expected: BigInt) =>
      lcm(numbers.map(BigInt(_))) should equal (expected)
    }
  }
}
