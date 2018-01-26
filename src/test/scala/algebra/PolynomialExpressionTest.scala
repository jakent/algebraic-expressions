package algebra

import org.scalatest.{FlatSpec, Matchers}

class PolynomialExpressionTest extends FlatSpec with Matchers {

  "Polynomial" should "add and multiply constants" in {
    Expression("(2x+2) + 6").toString shouldEqual "2x + 8"
    Expression("18*(2x+2)").toString shouldBe "36x + 36"
    Expression("18*(2x^2+2x+2)").toString shouldEqual "36x^2 + 36x + 36"
    Expression("18*(2x+2) + 5").toString shouldEqual "36x + 41"
  }

  it should "multiply by a negative constant" in {
    Expression("(2x+2) * -1").toString shouldEqual "-2x - 2"
  }

  it should "divide by a constant" in {
    Expression("6 * x / 2").toString shouldEqual "3x"
  }

  it should "add a variable with no like terms" in {
    Expression("(2x + 2) + 2x^2").toString shouldEqual "2x^2 + 2x + 2"
    Expression("2x^6 + (5x + 10)").toString shouldEqual "2x^6 + 5x + 10"
  }

  it should "add a variable with like terms" in {
    Expression("(2x + 2) + 2x").toString shouldEqual "4x + 2"
    Expression("2x + (5x + 10)").toString shouldEqual "7x + 10"
  }

  it should "add a polynomial" in {
    Expression("(2x + 2) + (2x^2 + 3)").toString shouldEqual "2x^2 + 2x + 5"
    Expression("(2x + 2) + (2x + 3)").toString shouldEqual "4x + 5"
  }

  it should "multiply by a variable" in {
    Expression("(2x + 2) * 2x^2").toString shouldEqual "4x^3 + 4x^2"
    Expression("2x^2 * (6x^2 + 2)").toString shouldEqual "12x^4 + 4x^2"
  }

  it should "multiply by another polynomial" in {
    Expression("(12x+10)*(2x+4)").toString shouldEqual "24x^2 + 68x + 40"
  }

  it should "subtract a constant" in {
    Expression("(12x+10) - 4").toString shouldEqual "12x + 6"
  }

  it should "subtract by a constant" in {
    Expression("4 - (12x+10)").toString shouldEqual "-12x - 6"
  }

  it should "blah" in {
    Expression("10x + 2x - (3x + 6)/3").toString shouldEqual "11x - 2"
  }

  it should "blah2" in {
    Expression("((9x + 81)/3 + 27)/3  - 2x").toString shouldEqual "-x + 18"
  }

  it should "blah3" in {
    Expression("18x + (12x + 10)*(2x+4)/2 - 5x").toString shouldEqual "12x^2 + 47x + 20"
  }

  it should "blah4" in {
    Expression("(2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1)  - 2x").toString shouldEqual "2x^3 + 23x^2 + 61x + 45"
  }

  it should "blah5" in {
    Expression("(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1) - 2x").toString shouldEqual "2x^5 + 5x^4 + 18x^2 + 61x + 45"
  }
}
