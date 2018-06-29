package com.willbeason.functions

/**
  * A polynomial of arbitrary order.
  *
  * The coefficients increase in order, so Polynomial(Seq(1,2,3)) is the same as
  * 3x^2^+2x+1. This convention allows the convenient notation
  * polynomial.coefficients(n) to refer to the n-th order coefficient.
  *
  * @param coefficients The polynomial's coefficients in increasing order.
  */
case class Polynomial(coefficients: Seq[Double] = Seq()) {
  /**
    * Evaluate this Polynomial at a specific value.
    *
    * @param x The value at which to evaluate the polynomial.
    * @return
    */
  def apply(x: Double): Double = coefficients.foldRight(0.0)(_ + x * _)

  /**
    * The order of this polynomial. By definition, the empty polynomial has order -1.
    */
  val order: Int = coefficients.length - 1

  def unary_+ : Polynomial = this

  /**
    * @return The additive negation of this polynomial.
    */
  def unary_- = Polynomial(coefficients.map(-_))

  /**
    * @param that The polynomial to add to this one.
    * @return The polynomial which is the sum of this and another polynomial.
    */
  def +(that: Polynomial): Polynomial =
    Polynomial(this.coefficients.zipAll(that.coefficients, 0.0, 0.0).map {
      case (thisCoefficient, thatCoefficient) => thisCoefficient + thatCoefficient
    })

  /**
    * Defined on its own rather than as a combination of the unary negation and
    * addition for performance reasons.
    *
    * @param that The polynomial to subtract from this one.
    * @return The polynomial which is the difference of this and another
    *         polynomial.
    */
  def -(that: Polynomial): Polynomial =
    Polynomial(this.coefficients.zipAll(that.coefficients, 0.0, 0.0).map {
      case (thisCoefficient, thatCoefficient) => thisCoefficient - thatCoefficient
    })

  /**
    * @param factor A constant by which to multiply this polynomial.
    * @return This polynomial multiplied by a constant.
    */
  def *(factor : Double): Polynomial = Polynomial(coefficients.map(factor * _))

  /**
    * Removes leading zeroes from this Polynomial.
    * @return An otherwise identical Polynomial, but without leading zeroes.
    */
  def normalized: Polynomial = Polynomial(coefficients.reverse.dropWhile(_ == 0.0).reverse)
}
