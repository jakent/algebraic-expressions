
trait ExpressionParser extends TermParser {
  def loop(acc: Term, b: List[~[String, Term]]): Term = b match {
    case h :: tail => {
      println("in loop", h)
      loop(h._1 match {
        case "+" => acc + h._2
        case "-" => acc - h._2
        case "*" => acc * h._2
        case "/" => acc / h._2
      }, tail)
    }
    case Nil => acc
  }

  def expr: Parser[Term] = term ~ rep("+" ~ term | "-" ~ term) ^^
    (b => loop(b._1, b._2))

  def term: Parser[Term] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^
    (b => loop(b._1, b._2))

  def factor: Parser[Term] = variable | "(" ~> expr <~ ")"
}

object Expression extends ExpressionParser {
  def apply(s: String) = {
    parseAll(expr, s) match {
      case Success(result, inl) => result
      case _ => "womp"
    }
  }
}
