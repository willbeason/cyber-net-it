package com.willbeason.functions

trait OrthogonalPolynomials {
  val polynomials: Int => Polynomial
  
  protected def weight(x: Double): Double
  
  protected def factor(order: Int): Double
  
  final def coefficient(order: Int, data: Seq[(Double, Double)]): Double =
    data.map { case (x, y) => y * polynomials(order)(x) * weight(x)}.sum / data.length
  
  final def fit(maxOrder: Int, data: Seq[(Double, Double)]): Polynomial =
    (0 to maxOrder).map { order => polynomials(order) * coefficient(order, data) * factor(order)}.reduce(_ + _)
}
