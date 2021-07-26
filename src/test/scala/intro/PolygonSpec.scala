package intro

import intro.Polygon.{area, distance}
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PolygonSpec extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {

  property("Euclidean distance evaluation") {
    val distances = Table(
      ("p0", "p1", "expectedDistance"),
      ((0, 0), (0, 1), 1.0),
      ((1043, 770), (551, 990), 538.947),
      ((551, 990), (681, 463), 542.797),
      ((681, 463), (1043, 770), 474.650)
    )
    forAll (distances) { (p0, p1, expectedDistance) =>
      distance(p0, p1) should equal (expectedDistance +- 0.001)
    }
  }

  property("Distance between coincident points should be 0") {
    forAll { (x0: Int, y0: Int, x1: Int, y1: Int) =>
      whenever(x0 != Int.MinValue && x0 != Int.MaxValue
        && x1 != Int.MinValue && x1 != Int.MaxValue
        && y0 != Int.MinValue && y0 != Int.MaxValue
        && y1 != Int.MinValue && y1 != Int.MaxValue) {
        val expectedDistance = distance((x0, y0), (x1, y1))
        if (x0 == x1 && y0 == y1) expectedDistance should equal(0.0)
        else expectedDistance > 0
      }
    }
  }

  property("Polygon area evaluation") {
    val polygons = Table(
      ("points", "expectedArea"),
      (IndexedSeq(
        (458, 695),
        (621, 483),
        (877, 469),
        (1035, 636),
        (1061, 825),
        (875, 1023),
        (645, 1033),
        (485, 853)),
        255931.0)
    )
    forAll (polygons) { (points, expectedArea) =>
      area(points) should equal (expectedArea)
    }
  }

}
