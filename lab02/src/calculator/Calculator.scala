// Laboratoire 02 - Calculatrice
// modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

import scala.io.Source
import calculator.parser.Parser

class Calculator(src: String) extends Parser {

  val source: Source = Source.fromString(src)

  def execute(): Unit = printTree

  def fatalError(msg: String): Nothing = {
    println("Fatal error", msg)
    sys.exit(1)
  }
}