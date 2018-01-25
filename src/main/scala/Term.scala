
sealed trait Operator
case object * extends Operator
case object / extends Operator
case object Plus extends Operator
case object Minus extends Operator

sealed trait Term {
  def *(t: Term): Term
  def /(t: Term): Term
  def +(t: Term): Term
  def -(t: Term): Term
}

case class Constant(i: Int) extends Term {
  def *(t: Term): Term = t match {
    case v: Variable => Variable(i * v.coefficient, v.exponent)
    case c: Constant => Constant(i * c)
  }

  def /(t: Term): Term = t match {
    case _: Variable => throw new Exception("Dividing constants by variables are not expected")
    case c: Constant => Constant(i / c)
  }

  def +(t: Term): Term = t match {
    case v: Variable => Polynomial((Plus, v), (Plus, this))
    case c: Constant => Constant(i + c)
  }

  def -(t: Term): Term = t match {
    case v: Variable => Polynomial((Plus, v), (Minus, this))
    case c: Constant => Constant(i - c)
  }
}

case class Variable(coefficient: Int, exponent: Int = 1) extends Term {
  def *(t: Term): Variable = t match {
    case v: Variable => Variable(coefficient * v.coefficient, exponent + v.exponent)
    case c: Constant => Variable(coefficient * c, exponent)
  }

  def /(t: Term): Term = t match {
    case v: Variable => exponent - v.exponent match {
      case 0 => Constant(coefficient / v.coefficient)
      case _ => Variable(coefficient / v.coefficient, exponent - v.exponent)
    }
    case c: Constant => Variable(coefficient / c, exponent)
  }

  def +(t: Term): Term = t match {
    case v: Variable => v.exponent == exponent match {
      case true => Variable(coefficient + v.coefficient, exponent)
      case false => Polynomial((Plus, this),(Plus, v))
    }
    case c: Constant => Polynomial((Plus, this), (Plus, c))
  }

  def -(t: Term): Term = ???
}

case class Polynomial(terms: (Operator, Term)*) extends Term {
  def *(t: Term): Term = throw new Exception("Multiplying polynomials were not expected")
  def /(t: Term): Term = throw new Exception("Dividing polynomials were not expected")
  def +(t: Term): Term = throw new Exception("Adding polynomials were not expected")
  def -(t: Term): Term = throw new Exception("Subtracting polynomials were not expected")
}

object Term extends ExpressionParser {
  def apply(s: String): Term = {
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
