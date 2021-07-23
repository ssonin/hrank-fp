package intro

import scala.annotation.tailrec
import scala.io.StdIn

object FunctionsOrNot extends App {

  val numCases = StdIn.readInt()
  (0 until numCases).foreach(_ => verify())

  def verify(): Unit = {
    val values = readInput(StdIn.readInt())
    val result = if (isFunction(values)) "YES" else "NO"
    println(result)
  }

  def readInput(size: Int): IndexedSeq[(String, String)] = {
    (0 until size).map {
      _ => StdIn.readf2("{0} {1}") match {
        case (k, v) => k.toString -> v.toString
      }
    }
  }

  def isFunction(values: IndexedSeq[(String, String)]): Boolean = {
    @tailrec
    def go(values: IndexedSeq[(String, String)], acc: Map[String, String]): Boolean = {
      if (values.isEmpty) true
      else {
        val (k, v) = values.head
        if (acc.isDefinedAt(k)) acc(k).equals(v) && go(values.tail, acc)
        else go(values.tail, acc + (k -> v))
      }
    }
    go(values, Map())
  }
}
