package com.willbeason

import scala.util.Random

package object random {

  def randomDoubles(): Stream[Double] = Stream.continually(Random.nextDouble)
}
