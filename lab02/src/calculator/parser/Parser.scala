// Laboratoire 02 - Calculatrice
// modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator.parser

import calculator.Main.memory
import calculator.Calculator
import calculator.lexer._

trait Parser extends Lexer {
  self: Calculator =>

  import Trees._
  import calculator.lexer.Tokens._

  def computeSource: Double = { readToken; parseExpr.compute }

  def printTree: Unit = { readToken; println(parseExpr) }

  /** Store the current token, as read from the lexer. */
  private var currentToken: Token = Token(BAD)

  /** update currentToken using nextToken in the Lexer. */
  def readToken: Unit = { currentToken = nextToken }

  /** ""Eats"" the expected token, or terminates with an error. */
  private def eat(tokenClass: TokenClass): Unit = if (tokenClass == currentToken.info.tokenClass) readToken else expected(tokenClass)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = fatalError("expected: " + (tokenClass :: more.toList).mkString(" or ") + ", found: " + currentToken)

  private def parseExpr: ExprTree = {
    parseEquals(null)
  }

  private def parseEquals(ex: ExprTree): ExprTree = {
    var e = parseSimpleExpr(ex) // You should modify this call, to respect operations priority !
    if (currentToken.info == EQSIGN) {
      eat(EQSIGN)
      e match {
        case id @ Identifier(_) => {
          val rhs = parseEquals(ex)
          rhs match {
            case Assign(_, _) => fatalError("Invalid variable declaration !")
            case _ => {
              memory += (id.value -> rhs.compute)
              Assign(id, rhs)
            }
          }
        }
        case _ => fatalError("Invalid variable declaration !")
      }
    } else {
      e
    }
  }

  private def parseSimpleExpr(ex: ExprTree): ExprTree = {
    currentToken.info match {
      case _ => expected(???)
    }
  }

  private def parseExprTreeToken[T <: ExprTree](retTree: T): ExprTree = {
    val ret = retTree
    readToken
    ret
  }
}

