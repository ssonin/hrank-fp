package adhoc

import scala.collection.immutable.ListSet
import scala.io.StdIn.readLine

object RemoveDuplicates {

  def main(args: Array[String]): Unit = {
    println(ListSet(readLine: _*).mkString)
  }
}
