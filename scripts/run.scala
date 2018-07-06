
import com.willbeason.functions._
import com.willbeason.random.randomDoubles
import com.willbeason.functions.ChebyshevPolynomialsSecondKind._

import scala.util.Random

val actualPolynomial = polynomials(0)*3 + polynomials(1)*2 + polynomials(2)*1

val i = 30

def generateData: Seq[(Double, Double)] =
 randomDoubles().take(i).map(_ * 2.0 - 1.0).map {
    d => d -> (actualPolynomial(d) + 1.0*(0.2 * Random.nextDouble() - 0.1))
  }


val data: Seq[(Double, Double)] = generateData

val fitter = new PolynomialFitter()

fitter.cost(ChebyshevPolynomialsFirstKind.fit(0, data), data)
fitter.cost(ChebyshevPolynomialsFirstKind.fit(1, data), data)
fitter.cost(ChebyshevPolynomialsFirstKind.fit(2, data), data)
fitter.cost(ChebyshevPolynomialsFirstKind.fit(3, data), data)
fitter.cost(ChebyshevPolynomialsFirstKind.fit(4, data), data)
fitter.cost(ChebyshevPolynomialsFirstKind.fit(5, data), data)
fitter.cost(ChebyshevPolynomialsFirstKind.fit(6, data), data)
fitter.cost(ChebyshevPolynomialsFirstKind.fit(7, data), data)

ChebyshevPolynomialsFirstKind.fit(2, data)

fitter.cost(ChebyshevPolynomialsSecondKind.fit(0, data), data)
fitter.cost(ChebyshevPolynomialsSecondKind.fit(1, data), data)
fitter.cost(ChebyshevPolynomialsSecondKind.fit(2, data), data)
fitter.cost(ChebyshevPolynomialsSecondKind.fit(3, data), data)
fitter.cost(ChebyshevPolynomialsSecondKind.fit(4, data), data)
fitter.cost(ChebyshevPolynomialsSecondKind.fit(5, data), data)
fitter.cost(ChebyshevPolynomialsSecondKind.fit(6, data), data)
fitter.cost(ChebyshevPolynomialsSecondKind.fit(7, data), data)

val p = ChebyshevPolynomialsSecondKind.fit(3, data)
fitter.cost(p, data)

val fitted = fitter.newtonFit(p, data, 1E-5)

fitter.cost(actualPolynomial, data)
fitter.cost(fitted, data)

data.map {
  case (x, y) => (x, y, fitted(x), Math.abs(y - fitted(x)))
}.sortBy(_._4).foreach(println)
