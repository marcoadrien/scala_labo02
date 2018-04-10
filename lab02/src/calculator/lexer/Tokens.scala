// Laboratoire 02 - Calculatrice
// modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator.lexer

import calculator.Positional

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

  /** Tokens */
  case object BAD extends TokenInfo with TokenClass // Represents incorrect tokens

  case object EOF extends TokenInfo with TokenClass // Represents end of file

  /** Insert other Tokens here */
  //--------------------------------------------------------------------------------------------------------------------
  case object PLUS extends TokenInfo with TokenClass // +

  case object MINUS extends TokenInfo with TokenClass // -

  case object TIMES extends TokenInfo with TokenClass // *

  case object DIV extends TokenInfo with TokenClass // /

  case object MOD extends TokenInfo with TokenClass // %

  case object POW extends TokenInfo with TokenClass // ^

  case object FACT extends TokenInfo with TokenClass // !

  case object EQSIGN extends TokenInfo with TokenClass // =

  case object LPAREN extends TokenInfo with TokenClass // (

  case object RPAREN extends TokenInfo with TokenClass // )

  case object COMMA extends TokenInfo with TokenClass // ,

  case object SQRT extends TokenInfo with TokenClass // Square Root

  case object GCD extends TokenInfo with TokenClass // Greatest Common Divisor

  case object EGCD extends TokenInfo with TokenClass // Extended Euclidean algorithm

  case object MODINV extends TokenInfo with TokenClass // Modular Inverse

  case class NUM(value: Double) extends TokenInfo with TokenClass { // Numeric value
    override def toString: String = s"NUM($value)"
  }

  case class ID(name: String) extends TokenInfo with TokenClass { // Memory variable
    override def toString: String = s"ID($name)"
  }
  //--------------------------------------------------------------------------------------------------------------------

  object Token {
    def apply(info: TokenInfo): Token = new Token(info)

    def unapply(token: Token): Option[TokenInfo] = Some(token.info)
  }

}