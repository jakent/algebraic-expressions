

def blah(i: Int, j: List[String]): Int = {
  def loop(acc: Int, b: List[String]): Int = b match {
    case h :: tail => loop(h.charAt(0) match {
      case '+' => acc + h.drop(1).toInt
      case '-' => acc - h.drop(1).toInt
    }, tail)
    case Nil => acc
  }

  loop(i, j)
}

blah(2, List("-1", "+4", "-5"))



"Hello".drop(1)