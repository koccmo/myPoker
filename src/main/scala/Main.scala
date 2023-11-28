import Solver.process
import domain.Card
import domain.Rank.Two
import domain.Suit.Hearts

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = Iterator.continually(Option(StdIn.readLine()))
    .takeWhile(_.nonEmpty)
    .foreach { x =>
      x map process foreach println
    }
}
