package com.willbeason.functions

import scalaz.Memo

object LegendrePolynomials extends OrthogonalPolynomials {
  lazy val polynomials: Int => Polynomial = Memo.immutableHashMapMemo {
    case 0 => Polynomial(Seq(1.0))
    case 1 => Polynomial(Seq(0.0, 1.0))
    case i => (polynomials(i - 1).x * (2*i + 1) - polynomials(i - 2)*i) / (2*i + 1)
  }
  
  override protected def weight(x: Double): Double = 1.0
  
  override protected def factor(order: Int): Double = 2.0*order + 1.0
}
