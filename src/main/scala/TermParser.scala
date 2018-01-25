import scala.util.parsing.combinator.JavaTokenParsers

trait TermParser extends JavaTokenParsers {
  val coefficient: Parser[Variable] =
    """\d*""".r <~ "x" ^^ {
      case "" => Variable(1)
      case n => Variable(n.toInt)
    }
  val exponent: Parser[Variable] = coefficient ~ "^" ~"""\d+""".r ^^ {
    case x ~ _ ~ e => Variable(x.coefficient, e.toInt)
  }
  val constant: Parser[Constant] = """-?\d+""".r ^^ (n => Constant(n.toInt))

  val variable: Parser[Term] = exponent | coefficient | constant


}