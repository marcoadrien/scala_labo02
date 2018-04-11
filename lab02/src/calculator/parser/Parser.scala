// Laboratoire 02 - Calculatrice
// modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator.parser

import calculator.Main.memory
import calculator.lexer._
import scala.io.Source

class Parser(source:Source) extends Lexer(source:Source) {


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
    parseEquals
  }

  private def parseEquals: ExprTree = {
    val e = parsePlusMinus
    if (currentToken.info == EQSIGN) {
      eat(EQSIGN)
      e match {
        case id @ Identifier(_) => {
          val rhs = parseEquals
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

  private def parseTimesDiv: ExprTree = {
    val e = parseMod
    if (currentToken.info == TIMES) {
      ???
    } else if (currentToken.info == DIV) {
      ???
    } else {
      e
    }
  }

  private def parseMod: ExprTree = {
    val e = parsePow
    if (currentToken.info == MOD) {
      ???
    } else {
      e
    }
  }

  private def parsePow: ExprTree = {
    val e = parseFact
    if (currentToken.info == POW) {
      ???
    } else {
      e
    }
  }

  private def parseFact: ExprTree = {
    val e = parseSimpleExpr
    if (currentToken.info == FACT) {
      ???
    } else {
      e
    }
  }


  private def parsePlusMinus: ExprTree = {
    var e = parseTimesDiv // You should modify this call, to respect operations priority !
    while (currentToken.info == PLUS || currentToken.info == MINUS) {
      if (currentToken.info == PLUS) {
        eat(PLUS)
        e = Plus(e, parseTimesDiv) // You should modify this call, to respect operations priority !
      } else {
        eat(MINUS)
        e = Minus(e, parseTimesDiv)
      }
    }
    e
  }


  private def parseSimpleExpr: ExprTree = {
    // Here you want to match simple expressions such as NUM(value) and parse them (for example with the parseExprTreeToken method).
    currentToken.info match {
      case LPAREN => parseParenthesis // Parenthesis
      case COMMA =>
        eat(COMMA)
        parsePlusMinus
      case NUM(value) =>
        eat(NUM(value))
        NumLit(stripDot(value.toString))
      case ID(name) =>
        eat(ID(name))
        Identifier(name)
      case SQRT =>
        eat(SQRT)
        Sqrt(parseParenthesis())
      case GCD =>
        eat(GCD)
        eat(LPAREN)
        val ret = Gcd(parsePlusMinus, parsePlusMinus)
        eat(RPAREN)
        ret
      case _      => expected(COMMA)
    }
  }

  private def stripDot(s: String) = if (s endsWith ".0") s.substring(0, s.length - 2) else s

  private def parseExprTreeToken[T <: ExprTree](retTree: T): ExprTree = {
    readToken
    retTree
  }

  private def parseParenthesis(): ExprTree = {
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(RPAREN)
    ret
  }
}



