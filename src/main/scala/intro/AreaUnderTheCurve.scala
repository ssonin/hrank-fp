package intro

import scala.math.{Pi, pow}

object AreaUnderTheCurve extends App {

  def f(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    require(coefficients.size == powers.size)
    coefficients.zip(powers).foldLeft(0.0){case (acc, (c, p)) => acc + c * pow(x, p)}
  }

  def area(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    val radius = f(coefficients, powers, x)
    Pi * pow(radius, 2)
  }

  def summation(func: (List[Int], List[Int], Double) => Double, upperLimit: Int, lowerLimit: Int,
                coefficients: List[Int], powers: List[Int]): Double = {
    val step = 0.001
    Range.BigDecimal.inclusive(BigDecimal(lowerLimit + step), BigDecimal(upperLimit), BigDecimal(step))
      .foldLeft(0.0){case (acc, i) => acc + func(coefficients, powers, i.toDouble) * step}
  }

  println(summation(f, 4, 1, (1 to 5).toList, (6 to 10).toList))
  println(summation(area, 4, 1, (1 to 5).toList, (6 to 10).toList))
}
