/*
Laboratoire 02 - Calculatrice
Modifications: Adrien Marco, Julien Brêchet, Loan Lassalle
*/

package calculator.utils

import scala.annotation.tailrec

/**
  * This class gives a normalization tool for a string or for a floating point number.
  */
object Utils {

  /**
    * Strips the zero and the dot at the end of a number as a string
    *
    * @param s number as a string
    * @return number as a string without dot and zero at the end
    */
  @tailrec
  def normalize(s: String): String  = {
    if (s.contains('.'))
      s.last match  {
        case '0' => normalize(s.substring(0, s.length - 1))
        case '.' => s.substring(0, s.length - 1)
        case _ => s
      }
    else
      s
  }

  /**
    * Normalize a floating point number by rounding value and cut after a number of decimals.
    *
    * @param x number to normalize
    * @param scale number of decimals
    * @return normalize floating point number
    */
  def normalize(x: Double, scale: Int = 10): Double =
    BigDecimal(x).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble

}
