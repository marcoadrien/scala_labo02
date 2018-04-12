// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

import calculator.parser.Parser
import scala.io.Source

class Calculator(source: Source) extends Parser(source: Source) {

  def execute(): Unit = printTree

// Next step after checking of older steps
//  def execute(): Unit = computeSource match {
//    case Double.NegativeInfinity => println("Memory updated !")
//    case result => println("Result : " + stripDot(result.toString))
//  }

  private def stripDot(s: String) = if (s endsWith ".0") s.substring(0, s.length - 2) else s
}
