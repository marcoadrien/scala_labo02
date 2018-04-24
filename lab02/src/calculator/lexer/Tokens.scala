/*
Laboratoire 02 - Calculatrice
Modifications: Adrien Marco, Julien Brêchet, Loan Lassalle
*/

package calculator.lexer

import calculator.Positional

/**
  * This class defines all the tokens we can use in this calculator.
  */
object Tokens {

  sealed trait TokenClass {
    self =>
    def tokenClass: self.type = self
  }

  sealed trait TokenInfo {
    def tokenClass: TokenClass
  }

  /** Token */
  class Token(val info: TokenInfo) extends Positional {
    override def toString: String = info.toString

    def tokenClass: TokenClass = info.tokenClass
  }

  object Token {
    def apply(info: TokenInfo): Token = new Token(info)

    def unapply(token: Token): Option[TokenInfo] = Some(token.info)
  }

  /** Tokens */
  case object PLUS extends TokenInfo with TokenClass // + (addition)

  case object MINUS extends TokenInfo with TokenClass // - (subtraction)

  case object TIMES extends TokenInfo with TokenClass // * (multiplication)

  case object DIV extends TokenInfo with TokenClass // / (division)

  case object MOD extends TokenInfo with TokenClass // % (modulo)

  case object POW extends TokenInfo with TokenClass // ^ (exponentiation)

  case object FACT extends TokenInfo with TokenClass // ! (factorial)

  case object EQSIGN extends TokenInfo with TokenClass // = (equal)

  case object LPAREN extends TokenInfo with TokenClass // ( left parenthesis

  case object RPAREN extends TokenInfo with TokenClass // ) right parenthesis

  case object COMMA extends TokenInfo with TokenClass // , comma

  case object SQRT extends TokenInfo with TokenClass // Square Root

  case object GCD extends TokenInfo with TokenClass // Greatest Common Divisor

  case object MODINV extends TokenInfo with TokenClass // Modular Multiplicative Inverse

  case class NUM(value: Double) extends TokenInfo with TokenClass { // Numeric value
    override def toString: String = s"NUM($value)"
  }

  case class ID(name: String) extends TokenInfo with TokenClass { // Memory variable
    override def toString: String = s"ID($name)"
  }

  case object BAD extends TokenInfo with TokenClass // Represents incorrect tokens

  case object EOF extends TokenInfo with TokenClass // Represents end of file

}
