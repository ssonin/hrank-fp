package recursion

import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.io.{Source, StdIn}
import scala.math.{pow, sqrt}

object ConvexHull {

  case class Point(x: Int, y: Int)

  object PointExtractor {
    def unapply(s: String): Option[Point] = try {
      Some({
        val tokens = s.split(" ")
        Point(tokens(0).toInt, tokens(1).toInt)
      })
    } catch {
      case _: NumberFormatException => None
    }
  }

  def main(args: Array[String]): Unit = {
//    require(args.length == 2)
//    val points = fromFile(args(0))
    val points = (0 until StdIn.readInt()).map(_ => readLine).map{ case PointExtractor(x) => x }
    val convexHull = grahamScan(points)
    println(perimeter(convexHull))
//    toFile(args(1), convexHull)
  }

  def grahamScan(points: Iterable[Point]): List[Point] = {

    @tailrec
    def walk(points: Iterable[Point], acc: List[Point]): List[Point] = {
      if (points.isEmpty) acc.reverse
      else {
        acc match {
          case curr :: prev :: _ if ccw(prev, curr, points.head) <= 0 =>
            walk(points, acc.tail)
          case _ => walk(points.tail, points.head :: acc)
        }
      }
    }

    val p0 = points.minBy(p => (p.y, p.x))
    val sorted = p0 :: (points.toSet - p0).toList.sortWith {
      (p1, p2) =>
        val zCoordinate = ccw(p0, p1, p2)
        if (zCoordinate != 0) zCoordinate > 0
        else (distance(p0, p1) - distance(p0, p2)) < 0
    }
    walk(sorted, Nil)
  }

  def ccw(p1: Point, p2: Point, p3: Point): Int =
    (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)

  def perimeter(points: Iterable[Point]): Double = {
    if (points.isEmpty) 0
    else points.zipAll(points.tail, points.head, points.head)
      .foldLeft(0.0){case (acc, (p1, p2)) => acc + distance(p1, p2)}
  }

  def distance(p1: Point, p2: Point): Double = sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2))

  def fromFile(filename: String): List[Point] = {
    val reader = Source.fromURL(getClass.getResource(filename))
    val lines = reader.getLines.toList
    reader.close()
    lines.tail.map { case PointExtractor(x) => x }
  }

  def toFile(pathname: String, points: Iterable[Point]): Unit = {
    val writer = Files.newBufferedWriter(Paths.get(pathname), StandardOpenOption.CREATE)
    for (point <- points) {
      writer.write(s"${point.x} ${point.y}${System.lineSeparator}")
    }
    writer.close()
  }
}
