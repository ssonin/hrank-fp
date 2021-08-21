package intro

import intro.CoverPointsWithSegments.{count, sort}
import org.scalacheck.Gen
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.mutable.ArrayBuffer
import scala.math.pow

class CoverPointsWithSegmentsSpec extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {

  val limit = 50_000
  val bound = pow(10, 8).toInt

  property("count") {
    val pointsGen = Gen.containerOfN[ArrayBuffer, Int](limit, Gen.choose(-bound, bound))
    val input = for {
      points <- pointsGen
      target <- Gen.choose(-bound, bound)
    } yield (points, target)

    forAll (input) {
      case (points, target) =>
        count(points.sorted, target) should equal (points.count(s => s <= target))
    }
  }

  property("quicksort3way") {
    val input = Gen.containerOfN[ArrayBuffer, Int](limit, Gen.choose(Int.MinValue, Int.MaxValue))
    forAll (input) { arr =>
      sort(arr) should equal (arr.sorted)
    }
  }

}
