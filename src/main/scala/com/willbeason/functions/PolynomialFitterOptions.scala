package com.willbeason.functions

object PolynomialFitterOptions {
  val DEFAULTS = PolynomialFitterOptions (2, 1000, 0.1, 0.0)
}

/**
  * @param polynomialOrder The highest power x is raised to in the polynomial.
  * @param maxIterations The largest number of times to iterate before giving up.
  * @param rate How quickly to approach a
  * @param momentumDecay
  */
case class PolynomialFitterOptions(
  polynomialOrder: Int,
  maxIterations: Int,
  rate: Double,
  momentumDecay: Double
) {

}
