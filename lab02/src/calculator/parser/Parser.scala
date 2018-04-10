// Laboratoire 02 - Calculatrice
// modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator.parser

import calculator.Calculator
import calculator.Main.memory
import calculator.lexer._

trait Parser extends Lexer {
  self: Calculator =>

  import Trees._
  import calculator.lexer.Tokens._

  /** Store the current token, as read from the lexer. */
  private var currentToken: Token = Token(BAD)

  def computeSource: Double = {
    readToken
    parseExpr.compute
  }

  def printTree: Unit = {
    readToken
    println(parseExpr)
  }

  /** ""Eats"" the expected token, or terminates with an error. */
  private def eat(tokenClass: TokenClass): Unit = if (tokenClass == currentToken.info.tokenClass) readToken else expected(tokenClass)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = fatalError("expected: " + (tokenClass :: more.toList).mkString(" or ") + ", found: " + currentToken)

  private def parseExpr: ExprTree = {
    parseEquals(null)
  }

  /** update currentToken using nextToken in the Lexer. */
  def readToken: Unit = {
    currentToken = nextToken
  }

  private def parseEquals(ex: ExprTree): ExprTree = {
    val e = parsePlusMinus(ex) // You should modify this call, to respect operations priority !
    if (currentToken.info == EQSIGN) {
      eat(EQSIGN)
      e match {
        case id@Identifier(_) =>
          val rhs = parseEquals(ex)
          rhs match {
            case Assign(_, _) => fatalError("Invalid variable declaration !")
            case _ =>
              memory += (id.value -> rhs.compute)
              Assign(id, rhs)
          }
        case _ => fatalError("Invalid variable declaration !")
      }
    } else {
      e
    }
  }

  private def parsePlusMinus(ex: ExprTree): ExprTree = {
    val e = parseTimesDiv(ex)
    if (currentToken.info == PLUS) {
      eat(PLUS)
      val rhs = parseEquals(ex)
      rhs match {
        case Assign(_, _) => fatalError("Invalid variable declaration !")
        case _ => Plus(e, rhs)
      }
    } else if (currentToken.info == MINUS) {
      eat(MINUS)
      val rhs = parseEquals(ex)
      rhs match {
        case Assign(_, _) => fatalError("Invalid variable declaration !")
        case _ => Minus(e, rhs)
      }
    } else {
      e
    }
  }

  private def parseTimesDiv(ex: ExprTree): ExprTree = {
    val e = parseMod(ex)
    if (currentToken.info == TIMES) {
      ???
    } else if (currentToken.info == DIV) {
      ???
    } else {
      e
    }
  }

  private def parseMod(ex: ExprTree): ExprTree = {
    val e = parsePow(ex)
    if (currentToken.info == MOD) {
      ???
    } else {
      e
    }
  }

  private def parsePow(ex: ExprTree): ExprTree = {
    val e = parseFact(ex)
    if (currentToken.info == POW) {
      ???
    } else {
      e
    }
  }

  private def parseFact(ex: ExprTree): ExprTree = {
    val e = parseSimpleExpr(ex)
    if (currentToken.info == FACT) {
      ???
    } else {
      e
    }
  }

  private def parseSimpleExpr(ex: ExprTree): ExprTree = currentToken.info match {
    case NUM(value) =>
      eat(NUM(value))
      NumLit(stripDot(value.toString))
    case ID(name) =>
      eat(ID(name))
      Identifier(name)
    case _ => expected(???)
  }

  private def parseExprTreeToken[T <: ExprTree](retTree: T): ExprTree = {
    val ret = retTree
    readToken
    ret
  }

  private def stripDot(s: String) = if (s endsWith ".0") s.substring(0, s.length - 2) else s
}

