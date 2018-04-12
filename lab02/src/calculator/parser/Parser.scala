// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator.parser

import calculator.Main.memory
import calculator.lexer._
import scala.io.Source

class Parser(source: Source) extends Lexer(source: Source) {

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

  /** update currentToken using nextToken in the Lexer. */
  def readToken: Unit = currentToken = nextToken

  /** ""Eats"" the expected token, or terminates with an error. */
  private def eat(tokenClass: TokenClass): Unit =
    if (tokenClass == currentToken.info.tokenClass)
      readToken
    else
      expected(tokenClass)

  /** Complains that what was found was not expected.
    * The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing =
    fatalError("expected: " + (tokenClass :: more.toList).mkString(" or ") + ", found: " + currentToken)

  private def parseExpr: ExprTree = {
    parseEquals
  }

  private def parseEquals: ExprTree = {
    val e = parsePlusMinus
    if (currentToken.info == EQSIGN) {
      eat(EQSIGN)
      e match {
        case id@Identifier(_) =>
          val rhs = parseEquals
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

  private def parsePlusMinus: ExprTree = {
    var e = parseTimesDiv
    while (currentToken.info == PLUS || currentToken.info == MINUS) {
      if (currentToken.info == PLUS) {
        eat(PLUS)
        e = Plus(e, parseTimesDiv)
      } else {
        eat(MINUS)
        e = Minus(e, parseTimesDiv)
      }
    }
    e
  }

  private def parseTimesDiv: ExprTree = {
    var e = parseMod
    while (currentToken.info == TIMES || currentToken.info == DIV) {
      if (currentToken.info == TIMES) {
        eat(TIMES)
        e = Times(e, parseMod)
      } else {
        eat(DIV)
        e = Div(e, parseMod)
      }
    }
    e
  }

  private def parseMod: ExprTree = {
    var e = parsePow
    while (currentToken.info == MOD) {
      eat(MOD)
      e = Mod(e, parsePow)
    }
    e
  }

  private def parsePow: ExprTree = {
    var e = parseFact
    while (currentToken.info == POW) {
      eat(POW)
      e = Pow(e, parseFact)
    }
    e
  }

  private def parseFact: ExprTree = {
    var e = parseSimpleExpr
    while (currentToken.info == FACT) {
      eat(FACT)
      e = Fact(e)
    }
    e
  }

  private def parseSimpleExpr: ExprTree = {
    currentToken.info match {
      case LPAREN => parseParenthesis // Parenthesis
      case COMMA =>
        eat(COMMA)
        parsePlusMinus
      case SQRT => parseKeyword(SQRT, Sqrt)
      case GCD => parseKeyword(GCD, Gcd)
      case MODINV => parseKeyword(MODINV, ModInv)
      case NUM(value) => parseExprTreeToken(NumLit(stripDot(value.toString)))
      case ID(name) => parseExprTreeToken(Identifier(name))
      case _ => expected(EOF, BAD)
    }
  }

  private def parseExprTreeToken[T <: ExprTree](retTree: T): ExprTree = {
    readToken
    retTree
  }

  private def parseParenthesis: ExprTree = {
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(RPAREN)
    ret
  }

  private def parseKeyword(tokenClass: TokenClass, f:(ExprTree) => ExprTree): ExprTree = {
    eat(tokenClass)
    f(parseParenthesis)
  }

  private def parseKeyword(tokenClass: TokenClass, f:(ExprTree, ExprTree) => ExprTree): ExprTree = {
    eat(tokenClass)
    eat(LPAREN)
    val ret = f(parsePlusMinus, parsePlusMinus)
    eat(RPAREN)
    ret
  }

  private def stripDot(s: String) = if (s endsWith ".0") s.substring(0, s.length - 2) else s

}

