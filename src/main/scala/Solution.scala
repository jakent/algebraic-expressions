import algebra.Expression

import scala.io.Source

object Solution {

  def main(args: Array[String]) {
    val lines = Source.stdin.getLines
    val nTests = lines.next.toInt

    lines.take(nTests).toSeq.map(Expression(_)).foreach(println)
  }
}