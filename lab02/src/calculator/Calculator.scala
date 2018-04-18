// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

import calculator.parser.Parser
import calculator.utils.Utils

import scala.io.Source

class Calculator(source: Source) extends Parser(source: Source) {

  def execute(): Unit =
    try {
      computeSource match {
        case Double.NegativeInfinity => println("Memory updated !")
        case result => println("Result : " + Utils.normalize(result))
      }
    } catch {
      case uoe: UnsupportedOperationException => println(uoe.getMessage)
    }

}
