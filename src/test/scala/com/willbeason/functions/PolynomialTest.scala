package com.willbeason.functions

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

import scala.util.Random

class PolynomialTest extends FeatureSpec with GivenWhenThen with Matchers {

  def randomDoubles(): Stream[Double] = Stream.continually(Random.nextDouble)

  feature("Apply") {
    scenario("Evaluating an empty polynomial") {
      Given("an empty polynomial")
      val p = Polynomial()

      When("evaluated at any value")
      val xs: Seq[Double] = randomDoubles().take(100).toSeq
      val results: Seq[Double] = xs.map(p.apply)

      Then("the result should be 0")
      results.forall(_ == 0.0)
    }

    scenario("Evaluating order-0 polynomials at a value") {
      Given("order-0 polynomials")
      val bs: Seq[Double] = randomDoubles().take(10)
      val ps: Seq[Polynomial] = bs.map(Seq(_)).map(Polynomial.apply)

      When("evaluated at a value")
      val x: Double = Random.nextDouble()
      val results: Seq[Double] = ps.map(_(x))

      Then("the result should be the constants")
      (bs zip results).foreach { case (b, result) => b shouldBe result +- 1E-3}
    }

    scenario("Evaluating an order-0 polynomial at any value") {
      Given("order-0 polynomials")
      val b: Double = Random.nextDouble()
      val p: Polynomial = Polynomial(Seq(b))

      When("evaluated at any value")
      val xs: Seq[Double] = randomDoubles().take(10)
      val results: Seq[Double] = xs.map(p(_))

      Then("the result should be the constant")
      results.foreach(_ shouldBe b +- 1E-3)
    }

    scenario("Evaluating an order-1 polynomial") {
      Given("an order-1 polynomial")
      val m: Double = Random.nextDouble()
      val b: Double = Random.nextDouble()
      val p = Polynomial(Seq(b, m))

      When("evaluated at any value")
      val xs: Seq[Double] = randomDoubles().take(10)
      val results: Seq[Double] = xs.map(p(_))

      Then("the result should be mx+b")
      (xs zip results).foreach {
        case (x, result) => result shouldBe (m*x + b) +- 1E-3
      }
    }
  }

}
