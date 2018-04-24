/*
Laboratoire 02 - Calculatrice
Modifications: Adrien Marco, Julien Brêchet, Loan Lassalle
*/

package calculator.lexer

import calculator.utils.Utils

import scala.io.Source

/**
  * This class reads the user input and annotates characters or words of a token
  * to the corresponding token.
  *
  * @param source user input
  */
class Lexer(source: Source) {

  import Tokens._

  var position: Int = 0
  var ch: Char = ' '
  var eof: Boolean = false

  val numeric: List[Char] = ('0' to '9').toList
  val alphabetic: List[Char] = ('a' to 'z').toList ++ ('A' to 'Z').toList
  val alphanumeric: List[Char] = numeric ++ alphabetic ++ List('_')

  /**
    * Works like an iterator and returns the next token from the input stream.
    *
    * @throws java.lang.UnsupportedOperationException is thrown if user input contains unsupported characters
    *                                                 or if number representation is not supported
    * @return token annotated to user input
    */
  @throws(classOf[UnsupportedOperationException])
  def nextToken: Token = {

    // Reads and annotates all character of user input until the end of file symbol
    if (eof) {
      position = source.pos
      setToken(EOF)
    } else {
      if (position == 0) nextChar
      position = source.pos
      ch match {
        case ' ' => skipToken
        case '+' => setToken(PLUS)
        case '-' => setToken(MINUS)
        case '*' => setToken(TIMES)
        case '/' => setToken(DIV)
        case '%' => setToken(MOD)
        case '^' => setToken(POW)
        case '!' => setToken(FACT)
        case '=' => setToken(EQSIGN)
        case '(' => setToken(LPAREN)
        case ')' => setToken(RPAREN)
        case ',' => setToken(COMMA)
        case _ =>
          // If it's not a simple character, it's necessary to read set of characters
          // up to the space character
          if (alphabetic.contains(ch)) {

            // It's a keyword or memory variable's identifier if the first character
            // is a character alphabetic
            Token(keywordOrId(readMultiple(alphanumeric)))
          } else if (numeric.contains(ch)) { //
            val value = Utils.normalize(readMultiple(numeric ++ "."))

            // Counts the number of dots to convert the string
            // into an integer or a floating point number
            value.count(_ == '.') match {
              case 0 => Token(NUM(value.toInt))
              case 1 => Token(NUM(value.toDouble))
              case _ =>
                throw new UnsupportedOperationException("Number representation unsupported")
            }
          } else {
            throw new UnsupportedOperationException("Character unsupported")
          }
      }
    }
  }

  /** Checks and set if the multiple Char found (str:String) is a keyword or a variable */
  def keywordOrId(str: String): TokenInfo = {
    str.toLowerCase match {
      case "sqrt" => SQRT // Square root
      case "gcd" => GCD // Greatest common divisor
      case "modinv" => MODINV // Modular multiplicative inverse
      case _ => ID(str.toLowerCase()) // Memory variable's identifier
    }
  }

  /** Moves the iterator to the next Char of the input source */
  def nextChar: Unit = if (source.hasNext) ch = source.next() else {
    ch = ' '; eof = true
  }

  /** Moves the iterator to the next Char and set previous Token */
  def setToken(tkn: TokenInfo): Token = {
    nextChar; Token(tkn).setPos(position)
  }

  /** Moves the iterator to the next Char and skip the current token, useful for empty Char */
  def skipToken: Token = {
    nextChar; nextToken
  }

  /** Reads multiple Char at once, useful for detecting variables and keywords */
  def readMultiple(allowed: List[Char]): String = {
    var str = "" + ch
    nextChar
    while (allowed.contains(ch) && !eof) {
      str += ch
      nextChar
    }
    str
  }

  def fatalError(msg: String): Nothing = {
    println("Fatal error", msg)
    sys.exit(1)
  }

}
