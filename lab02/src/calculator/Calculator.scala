// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

import calculator.parser.Parser
import calculator.utils.Utils

import scala.io.Source

class Calculator(source: Source) extends Parser(source: Source) {

  val scale = 10

 // TODO: Utiliser des constantes pour définir une réponse
  def execute(): Unit = computeSource match {
    case Double.NegativeInfinity => println("Memory updated !")
    case result =>
      val resultNormalized = BigDecimal(result).setScale(scale, BigDecimal.RoundingMode.HALF_UP)
      println("Result : " + Utils.normalize(resultNormalized.toString))
  }

}
