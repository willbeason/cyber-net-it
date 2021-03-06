package com.willbeason.functions

import scalaz.Memo

object ChebyshevPolynomialsFirstKind extends OrthogonalPolynomials {
  lazy val polynomials: Int => Polynomial = Memo.immutableHashMapMemo {
    case 0 => Polynomial(Seq(1.0))
    case 1 => Polynomial(Seq(0.0, 1.0))
    case i => polynomials(i - 1).x * 2 - polynomials(i - 2)
  }
  
  override protected def weight(x: Double): Double = 1.0 / Math.pow(1.0 - Math.pow(x, 2.0), 0.5)
  
  override protected def factor(i: Int): Double =
    if (i == 0) 2.0 / Math.PI
    else 4.0 / Math.PI
}
