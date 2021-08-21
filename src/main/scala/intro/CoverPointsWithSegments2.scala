package intro

import intro.ElemType.Type

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

/**
 * https://stepik.org/lesson/13249/step/6?unit=3434
 */
object CoverPointsWithSegments2 {

  def main(args: Array[String]): Unit = {
    val (numSegments, numPoints) = readIntTuple
    val inputData = new ArrayBuffer[Elem](numSegments + numPoints)
    for (_ <- 0 until numSegments) {
      val (left, right) = readIntTuple
      inputData += LineEnd(left, ElemType.Left)
      inputData += LineEnd(right, ElemType.Right)
    }
    val points = StdIn.readLine().split(" ")
    for (i <- 0 until points.length) {
      inputData += Point(points(i).toInt, ElemType.Point, i)
    }

    var cnt = 0
    val answer = new Array[Int](numPoints)
    for (elem <- inputData.sorted) { elem match {
      case LineEnd(_, ElemType.Left) => cnt += 1
      case LineEnd(_, ElemType.Right) => cnt -= 1
      case Point(_, _, pos) => answer(pos) = cnt
    }}
    println(answer.mkString(" "))
  }

  def readIntTuple: (Int, Int) = {
    StdIn.readf2("{0} {1}") match {
      case (n, m) => (n.toString.toInt, m.toString.toInt)
    }
  }
}

sealed trait Elem extends Ordered[Elem] {
  def x: Int
  def pointType: Type
  def compare(that: Elem): Int = {
    val result = this.x - that.x
    if (result != 0) result
    else this.pointType.compare(that.pointType)
  }
}

case class LineEnd(x: Int, pointType: Type) extends Elem
case class Point(x: Int, pointType: Type, pos: Int) extends Elem

object ElemType extends Enumeration {
  type Type = Value
  val Left, Point, Right = Value
}