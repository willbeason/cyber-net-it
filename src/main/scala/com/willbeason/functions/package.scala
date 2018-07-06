package com.willbeason

import com.willbeason.random.randomDoubles

package object functions {
  def randomPolynomial(order: Int): Polynomial = Polynomial(randomDoubles().take(order + 1))
}
