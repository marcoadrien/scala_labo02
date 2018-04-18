package calculator.utils

import scala.annotation.tailrec

object Utils {

  /**
    * Strips the zero and the dot at the end of a number as a string
    * @param s number as a string
    * @return number as a string without dot and zero at the end
    */
  @tailrec
  def normalize(s: String): String =
    if (s.contains('.') && s.endsWith("0"))
      normalize(s.substring(0, s.length - 1))
    else if (s.contains('.') && s.endsWith("."))
      s.substring(0, s.length - 1)
    else
      s

}
