package algebra

sealed trait Expression {
  def *(t: Expression): Expression
  def /(t: Expression): Expression
  def +(t: Expression): Expression
  def -(t: Expression): Expression
}

case class Constant(i: Int) extends Expression {
  def *(e: Constant): Constant = Constant(i * e) // TODO: figure out how to not need this
  def *(e: Expression): Expression = e match {
    case v: Variable => Variable(i * v.coefficient, v.exponent)
    case c: Constant => Constant(i * c)
    case p: Polynomial => p * i
  }

  def /(e: Expression): Expression = e match {
    case _: Variable => throw new Exception("Dividing constants by variables are not expected")
    case c: Constant => Constant(i / c)
    case p: Polynomial => throw new Exception("Dividing constants by polynomials are not expected")
  }

  def +(c: Constant): Constant = Constant(i + c)
  def +(e: Expression): Expression = e match {
    case v: Variable => Polynomial(i, Set(v))
    case c: Constant => this + c
    case p: Polynomial => p + i
  }

  def -(e: Expression): Expression = e match {
    case v: Variable => Polynomial(Math.negateExact(i), Set(v))
    case c: Constant => Constant(i - c)
    case p: Polynomial => p - i
  }
}

case class Variable(coefficient: Int, exponent: Int = 1) extends Expression {
  def *(e: Expression): Variable = e match {
    case v: Variable => Variable(coefficient * v.coefficient, exponent + v.exponent)
    case c: Constant => Variable(coefficient * c, exponent)
  }

  def /(e: Expression): Expression = e match {
    case v: Variable if exponent - v.exponent == 0 => Constant(coefficient / v.coefficient)
    case v: Variable => Variable(coefficient / v.coefficient, exponent - v.exponent)
    case c: Constant => Variable(coefficient / c, exponent)
  }

  def +(e: Expression): Expression = e match {
    case v: Variable if v.exponent == exponent => Variable(coefficient + v.coefficient, exponent)
    case v: Variable => Polynomial(0, Set(this, v))
    case c: Constant => Polynomial(c, Set(this))
  }

  def -(e: Expression): Expression = ???
}

case class Polynomial(constant: Constant, exponents: Set[Variable]) extends Expression {
  def *(e: Expression): Expression = e match {
    case v: Variable => ???
    case c: Constant => Polynomial(constant * c, exponents.map(_ * c))
//    case p: Polynomial => Polynomial(constant * p.constant, exponents.map())
  }

  def /(e: Expression): Expression = throw new Exception("Dividing polynomials were not expected")

  def +(e: Expression): Expression = e match {
    case v: Variable if exponents.exists(_.exponent == v.exponent) =>
      Polynomial(constant, exponents.map {
          case ee if ee.exponent == v.exponent => Variable(0) //e + blah
          case ee => ee
        })
    case v: Variable => Polynomial(constant, exponents + v)
    case c: Constant => Polynomial(constant + c, exponents)
  }

  def -(e: Expression): Expression = throw new Exception("Subtracting polynomials were not expected")
}

object Expression extends ExpressionParser {
  def apply(s: String): Expression = {
    val value = parseAll(expr, s)
    println(value)
    value match {
      case Success(result, inl) => result
      case _ => throw new Exception("womp")
    }
  }

  implicit def constantToInt(f: Constant): Int = f.i
  implicit def intToConstant(f: Int): Constant = Constant(f)
}
