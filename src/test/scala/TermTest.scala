import org.scalatest.{FlatSpec, Matchers}
import Term._

class TermTest extends FlatSpec with Matchers {

  "X" should "parse simple multiplier" in {
    Term("x") shouldEqual Variable(1)
    Term("3x") shouldEqual Variable(3)
    Term("1001x") shouldEqual Variable(1001)
  }

  it should "parse simple exponent" in {
    Term("x^2") shouldEqual Variable(1, 2)
    Term("3x^2") shouldEqual Variable(3, 2)
    Term("300x^21") shouldEqual Variable(300, 21)
  }

  it should "multiply" in {
    Term("x") * Term("4x") shouldEqual Variable(4, 2)
    Term("10x") * Term("4x") shouldEqual Variable(40, 2)
    Term("10x") * 4 shouldEqual Variable(40)
    4 * Variable(10, 3) shouldEqual Variable(40, 3)
    Variable(5, 10) * Variable(4, 10) shouldEqual Variable(20, 20)
  }

  it should "divide" in {
    Term("4x") / Term("2x") shouldEqual Constant(2)
    Term("4x") / Constant(2) shouldEqual Variable(2)
    Constant(4) / Constant(2) shouldEqual Constant(2)
    Term("10x^2") / Term("5x") shouldEqual Term("2x")
  }

  it should "add" in {
    Term("4x") + Term("4x") shouldEqual Term("8x")
    Term("4x^2") + Term("4x") shouldEqual Polynomial((Plus, Term("4x^2")),(Plus, Term("4x")))
    Term("4x^2") + Term("4x") shouldEqual Term("4x^2 + 4x")
  }
}
