package algebra

import org.scalatest.{FlatSpec, Matchers}

class PolynomialExpressionTest extends FlatSpec with Matchers {

  "Polynomial" should "add and multiply constants" in {
    Expression("(2x+2) + 6") shouldEqual Expression("2x + 8")
    Expression("18*(2x+2)") shouldEqual Expression("36x + 36")
    Expression("18*(2x^2+2x+2)") shouldEqual Expression("36x^2 + 36x + 36")
    Expression("18*(2x+2) + 5") shouldEqual Expression("36x + 41")
  }

  it should "divide by a constant" in {
    Expression("6 * x / 2") shouldEqual Expression("3x")
  }

  it should "add a variable" in {
    Expression("(2x + 2) + 2x^2") shouldEqual Expression("2x^2 + 2x + 2")
  }

  ignore should "multiply by another polynomial" in {
    Expression("(12x+10)*(2x+4)") shouldEqual Expression("24x^2 + 68x + 40")
  }
}
