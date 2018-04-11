// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

import calculator.parser.Parser
import scala.io.Source

class Calculator(source: Source) extends Parser(source: Source) {

  def execute(): Unit = printTree

}
