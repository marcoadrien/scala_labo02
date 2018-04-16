package calculator.utils

object Utils {

  /**
    * Strips the ".0" of a number as a string
    * @param s number as a string
    * @return number as a string without .0 at the end
    */
  def stripDot(s: String): String = if (s endsWith ".0") s.substring(0, s.length - 2) else s
}
