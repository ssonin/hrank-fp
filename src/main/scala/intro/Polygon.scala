package intro

import scala.io.StdIn
import scala.math.{pow, sqrt}

object Polygon extends App {

  type Point = (Int, Int)
  type Edge = (Point, Point)

  val points = (0 until StdIn.readInt()).map(_ => StdIn.readf2("{0} {1}") match {
    case (x, y) => (x.toString.toInt, y.toString.toInt)
  })
  println(perimeter(points))


  def perimeter(points: IndexedSeq[Point]): Double = {
    f(distance)(points)
  }

  def distance(p0: Point, p1: Point): Double = (p0, p1) match {
    case ((x0, y0), (x1, y1)) => sqrt(pow(x1 - x0, 2) + pow(y1 - y0, 2))
  }

  def edges(points: IndexedSeq[Point]): IndexedSeq[Edge] =
    points.zipAll(points.tail, points.head, points.head)

  def area(points: IndexedSeq[Point]): Double = {
    val result = f(areaUnderSegment)(points)
    if (result < 0) -result else result
  }

  def areaUnderSegment(p0: Point, p1: Point): Double = (p0, p1) match {
    case ((x0, y0), (x1, y1)) => (y0 + y1) / 2.0 * (x1 - x0)
  }

  private def f(op: (Point, Point) => Double): IndexedSeq[Point] => Double = {
    points => edges(points).foldLeft(0.0) { case (acc, (p0, p1)) => acc + op(p0, p1) }
  }
}
