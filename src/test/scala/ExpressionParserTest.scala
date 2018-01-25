import org.scalatest.{FlatSpec, Matchers}

class ExpressionParserTest extends FlatSpec with Matchers {

  // TODO: Add implicit conversions for constants
  "Expression" should "do simple addition" in {
    Expression("3 + 7") shouldEqual Term("10")
  }

  it should "do simple subtraction" in {
    Expression("3 - 1") shouldEqual Term("2")
  }

  it should "do simple addition and subtraction" in {
    Expression("7 + 3 - 1 + 100") shouldEqual Term("109")
  }

  it should "do simple multiplication" in {
    Expression("1 * 2") shouldEqual Term("2")
  }

  it should "multiply do it all" in {
    Expression("1 + 2 * 2 * 100 - 30") shouldEqual Term("371")
  }

  it should "handle parenthesis" in {
    Expression("1 + 2 * 2 * (100 - 100)") shouldEqual Term("1")
  }

  it should "handle multiple parenthesis" in {
    Expression("(1 + 3) / (2 * (100 - 98))") shouldEqual Term("1")
  }

  it should "do division" in {
    Expression("6 / 2") shouldEqual Term("3")
  }

  it should "handle a single variable" in {
    Expression("x + 6") shouldEqual Polynomial((Plus, Variable(1)), (Plus, Constant(6)))
  }

  it should "handle a multiplying variable" in {
    Expression("x * 6") shouldEqual Term("6x")
    Expression("6 * x") shouldEqual Term("6x")
  }

//  it should "handle a dividing variable" in {
//    Expression("6 * x / 2") shouldEqual "3x"
//  }
}
