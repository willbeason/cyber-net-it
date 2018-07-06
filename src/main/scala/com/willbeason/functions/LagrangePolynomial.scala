package com.willbeason.functions

object LagrangePolynomial {
  
  def fit(points: Map[Double, Double]): Polynomial = {
    points.map {
      case (x, y) =>
        points.filterKeys(_ != x).map {
          case (xn, _) => Polynomial(Seq(-xn, 1.0)) / (x - xn)
        }.fold(Polynomial(Seq(1.0)))(_ * _) * y
    }.fold(Polynomial(Seq(0.0)))(_ + _)
  }
  
}
