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

  /** Tokens */
  case object BAD extends TokenInfo with TokenClass       // represents incorrect tokens.
  case object EOF extends TokenInfo with TokenClass       // represents end of file
  /** Insert other Tokens here */
  case object EQSIGN extends TokenInfo with TokenClass    // =
  //--------------------------------------------------------------------------------------------------------------------
  case object LPAREN extends TokenInfo with TokenClass     // (
  case object RPAREN extends TokenInfo with TokenClass     // )
  case object PLUS extends TokenInfo with TokenClass        // +
  case object MINUS extends TokenInfo with TokenClass       // -
  case object TIMES extends TokenInfo with TokenClass         // *
  case object DIV extends TokenInfo with TokenClass         // /
  case object MOD extends TokenInfo with TokenClass         // %
  case object POW extends TokenInfo with TokenClass         // ^
  case object FACT extends TokenInfo with TokenClass         // !
  case object COMMA extends TokenInfo with TokenClass        // ,

  case object GCD extends TokenInfo with TokenClass         // gcd
  case object QSOLVE extends TokenInfo with TokenClass       // quadratic solve
  case object SQRT extends TokenInfo with TokenClass        // sqrt
  case object PRIME extends TokenInfo with TokenClass     // prime number
  case object EGCD extends TokenInfo with TokenClass        // egcd
  case object MODINV extends TokenInfo with TokenClass   // modInvert

  // memory variable
  case class ID(name:String) extends TokenInfo with TokenClass{
    override def toString: String = s"ID($name)"
  }
  // numeric value
  case class NUM(value:Int) extends TokenInfo with TokenClass{
    override def toString: String = s"NUM($value)"
  }
  //--------------------------------------------------------------------------------------------------------------------

  /** Token */
  class Token(val info: TokenInfo) extends Positional {
    override def toString: String = info.toString
    def tokenClass: TokenClass = info.tokenClass
  }

  object Token {
    def apply(info: TokenInfo): Token = new Token(info)
    def unapply(token: Token): Option[TokenInfo] = Some(token.info)
  }
}