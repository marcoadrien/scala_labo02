// Laboratoire 02 - Calculatrice
// modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

import scala.io.Source
import calculator.parser.Parser

class Calculator(source: Source) extends Parser (source:Source) {


  def execute(): Unit = printTree

}





