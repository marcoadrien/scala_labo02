/*
Laboratoire 02 - Calculatrice
Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
This class represents the calculator. It can execute calculations and print result messages like the solution or
calculation errors.
*/
package calculator

import calculator.parser.Parser
import calculator.utils.Utils

import scala.io.Source

class Calculator(source: Source) extends Parser(source: Source) {

  def execute(): Unit =
      /*
     we try to print the result (memory assignement or simple calculation)
     otherwise we catch the calculation exception (division by zero, negative square root and others)
     to print the good message
     */
    try {
      computeSource match {
        case Double.NegativeInfinity => println("Memory updated !")
        case result => println("Result : " + Utils.normalize(result))
      }
    } catch {
      case uoe: UnsupportedOperationException => println(uoe.getMessage)//the program wont stop (only if a fatal error is launched: see Lexer.scala)
    }

}
