package com.willbeason.functions

import com.willbeason.random.randomDoubles

object PolynomialFitter {
  implicit class DoubleArray(array: Seq[Double]) {
    def +(that: Seq[Double]): Seq[Double] = array.zipAll(that, 0.0, 0.0).map { case (a, b) => a + b }
    def *(factor: Double): Seq[Double] = array.map(factor * _)
    def norm: Double = array.map(Math.pow(_, 2)).sum
  }
}

class PolynomialFitter(options: PolynomialFitterOptions = PolynomialFitterOptions.DEFAULTS) {
  
  def cost(prediction: Double, actual: Double): Double =
    Math.pow(prediction - actual, 2)
  
  def cost(p: Polynomial, pairs: Seq[(Double, Double)]): Double =
    pairs.map { case (x, y) => cost(p(x), y) }.sum
  
  def fitParam(f: Double => Double, guess: Double, minStep: Double): Double = {
    var current: Double = guess
    var curCost: Double = f(current)
    
    var min: Double = current - minStep
    var minCost: Double = f(min)
    
    var max: Double = current + minStep
    var maxCost: Double = f(max)
    
    var converged = false
  
    var curStep: Double = minStep
    while (!converged) {
//      println(min, minCost)
//      println(current, curCost)
//      println(max, maxCost)
//      println(curStep)
      
      if (curCost <= minCost && curCost <= maxCost && curStep < (2.0*minStep)) {
        converged = true
      } else if (minCost < curCost && minCost < maxCost) {
        current = min
        curStep *= 2.0
      } else if (maxCost < curCost && maxCost < minCost) {
        current = max
        curStep *= 2.0
      } else {
        curStep /= 2.0
      }
  
      min = current - curStep
      max = current + curStep
      
      curCost = f(current)
      minCost = f(min)
      maxCost = f(max)
    }
    
    current
  }
  
  def fit(seed: Polynomial, pairs: Seq[(Double, Double)]): Polynomial = {
    
    var result = seed
    
    (0 to 100).foreach {
      _ =>
        (0 to result.order).foreach {
          order =>
            val d = fitParam(
              x => cost(result + Polynomial(Seq().padTo(order, 0.0) :+ x), pairs),
              0.0,
              0.01
            )
            result += Polynomial(Seq().padTo(order, 0.0) :+ d)
        }
    }
    
    result
  }
  
  def quadraticMin(y0: Double, y1: Double, y2: Double, resolution: Double = 1.0, offset: Double = 0.0): Double = {
  
    val a = (y2 + y0 - 2*y1) / (2.0 * resolution * resolution)
    val b = (y2 - y0) / (2.0 * resolution) - (2.0 * a * offset)
    
    - b / (2.0 * a)
  }
  
  def newtonFit(seed: Polynomial, pairs: Seq[(Double, Double)], resolution: Double): Polynomial = {
    
    var result = seed
    
    (0 to 100).foreach {
      _ =>
        val y1 = cost(result, pairs)
        
        val deltaP = Polynomial((0 to result.order).map {
          order =>
            val deltaC = Polynomial(Seq().padTo(order, 0.0) :+ resolution)
            
            val y0 = cost(result + deltaC, pairs)
            val y2 = cost(result - deltaC, pairs)
  
            quadraticMin(y0, y1, y2, resolution)
        })
        
        val y2 = cost(result - deltaP, pairs)
        val y3 = cost(result - deltaP*2.0, pairs)
        
        var c = quadraticMin(y1, y2, y3, 1.0, 1.0)
        c = Math.max(Math.min(c, 2.0), 0.0)
        
        result -= deltaP * c
    }
    
    result
  }
  
  def cubicRoots(p: Polynomial): Seq[Double] = {
    require(p.order == 3)
    
    val a = p(3)
    val b = p(2)
    val c = p(1)
    val d = p(0)
    
    val discriminant0 = b*b - 3*a*c
    val discriminant1 = 2*b*b*b - 9*a*b*c + 27*a*a*d
    
    
    
    Seq()
  }
  
  def quarticMin(p0: (Double, Double), p1: (Double, Double), p2: (Double, Double), p3: (Double, Double), p4: (Double, Double)): Double = {
    val quartic: Polynomial = LagrangePolynomial.fit(Seq(p0, p1, p2, p3, p4).toMap)
    
    val derivative: Polynomial = quartic.derivative
    
    
    
    0.0
  }
  
}
