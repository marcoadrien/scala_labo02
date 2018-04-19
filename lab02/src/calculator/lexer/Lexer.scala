/*
Laboratoire 02 - Calculatrice
Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
This class reads the input and assign the different tokens.
*/
package calculator.lexer

import calculator.utils.Utils

import scala.io.Source

class Lexer(source: Source) {

  import Tokens._

  var position: Int = 0
  var ch: Char = ' '
  var eof: Boolean = false

  val numeric: List[Char] = ('0' to '9').toList
  val alphabetic: List[Char] = ('a' to 'z').toList ++ ('A' to 'Z').toList
  val alphanumeric: List[Char] = numeric ++ alphabetic ++ List('_')

  /** Works like an iterator, and returns the next token from the input stream. */
  def nextToken: Token = {
    //end of file
    if (eof) {
      position = source.pos
      setToken(EOF)
    } else { //otherwise we read the character
      if (position == 0) nextChar
      position = source.pos
      ch match {
        case ' ' => skipToken//we don't use it
        //--------------------------------------------------------------------------------------------------------------------
        //here are the classic tokens we can find in the calculator
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
        //if it is not a classic token, it can be a memory variable, a numeric value or an upgraded function
        //like square root for example. And we have to read a set of characters (readMultiple()) to extract the value
        //that make more sense
        case _ =>
          //only alphabetic => upgraded function (keyword) or memory variable (ID)
          if (alphabetic.contains(ch)) {
            Token(keywordOrId(readMultiple(alphanumeric)))
          }
          //otherwise => numeric value  (Int or Double)
          else if (numeric.contains(ch)) {
            val value = Utils.normalize(readMultiple(numeric ++ "."))
            value.count(_ == '.') match {
              case 0 => Token(NUM(value.toInt))
              case 1 => Token(NUM(value.toDouble))//if we find a '.' => it is a double, if we find more => error
              case _ =>
                throw new UnsupportedOperationException("Invalid numeric")
            }
          }
          //if unknown character => error
          else {
            throw new UnsupportedOperationException("Invalid character")
          }
        //--------------------------------------------------------------------------------------------------------------------
      }
    }
  }

  /** Checks and set if the multiple Char found (str:String) is a keyword or a variable */
  def keywordOrId(str: String): TokenInfo = {
    str.toLowerCase match {
      case "sqrt" => SQRT //square root
      case "gcd" => GCD //biggest common divisor
      case "modinv" => MODINV //modulo inverse
      case _ => ID(str.toLowerCase()) //memory variable name (ID)
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
