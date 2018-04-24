/*
Laboratoire 02 - Calculatrice
Modifications: Adrien Marco, Julien Brêchet, Loan Lassalle
*/

package calculator

import calculator.parser.Parser
import calculator.utils.Utils

import scala.io.Source

/**
  * This class represents the calculator. It can execute calculations and display messages
  * like the result of user input or calculation errors.
  *
  * @param source user input
  */
class Calculator(source: Source) extends Parser(source: Source) {

  /**
    * Prints the result, memory assignment or simple calculation, otherwise
    * a calculation exception like division by zero or negative square root is catch and display the
    * correct message
    */
  def execute(): Unit =
    try {
      computeSource match {
        case Double.NegativeInfinity => println("Memory updated!")
        case result => println("Result : " + Utils.normalize(result))
      }
    } catch {
      // The program wont stop (only if a fatal error is launched: see Lexer.scala)
      case uoe: UnsupportedOperationException => println(uoe.getMessage)
    }

}
