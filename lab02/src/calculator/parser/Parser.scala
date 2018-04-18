// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator.parser

import calculator.Main.memory
import calculator.lexer._
import calculator.utils.Utils

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

  /**
    * Parses any expression
    *
    * @return result of expression
    */
  private def parseExpr: ExprTree = parseEquals

  /**
    * Parses an equal expression
    *
    * @return result of equal expression
    */
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

  /**
    * Parses an addition or subtraction expression
    *
    * @return result of addition or subtraction expression
    */
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

  /**
    * Parses an multiplication or division expression
    *
    * @return result of multiplication or division expression
    */
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

  /**
    * Parses an modular expression
    *
    * @return result of modular expression
    */
  private def parseMod: ExprTree = {
    var e = parsePow
    while (currentToken.info == MOD) {
      eat(MOD)
      e = Mod(e, parsePow)
    }
    e
  }

  /**
    * Parses an exponential expression
    *
    * @return result of exponential expression
    */
  private def parsePow: ExprTree = {
    var e = parseFact
    while (currentToken.info == POW) {
      eat(POW)
      e = Pow(e, parseFact)
    }
    e
  }

  /**
    * Parses an factorial expression
    *
    * @return result of factorial expression
    */
  private def parseFact: ExprTree = {
    var e = parseSimpleExpr
    while (currentToken.info == FACT) {
      eat(FACT)
      e = Fact(e)
    }
    e
  }

  /**
    * Parses a simple expression like keyword or number
    *
    * @return result of simple expression
    */
  private def parseSimpleExpr: ExprTree = {
    currentToken.info match {
      case LPAREN => parseParenthesis // Parenthesis
      case COMMA =>
        eat(COMMA)
        parsePlusMinus
      case SQRT => parseKeyword(Sqrt)
      case GCD => parseKeyword(Gcd)
      case MODINV => parseKeyword(ModInv)
      case PLUS => parseUnaryOperator
      case MINUS => parseUnaryOperator
      case NUM(value) => parseExprTreeToken(NumLit(Utils.normalize(value.toString)))
      case ID(name) => parseExprTreeToken(Identifier(name))
      case _ => expected(EOF, BAD)
    }
  }

  /**
    * Reads the next token and executes a parse function
    *
    * @param retTree parse function to execute
    * @tparam T type of function parse's return
    * @return reTree
    */
  private def parseExprTreeToken[T <: ExprTree](retTree: T): ExprTree = {
    readToken
    retTree
  }

  /**
    * Parses an expression between parenthesis
    *
    * @return result of expression between parenthesis
    */
  private def parseParenthesis: ExprTree = {
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(RPAREN)
    ret
  }

  /**
    * Parses a keyword as a function with one operand
    *
    * @param f function with one parameter as expression
    * @return function f's return
    */
  private def parseKeyword(f: (ExprTree) => ExprTree): ExprTree = {
    eat(currentToken.tokenClass)
    f(parseParenthesis)
  }

  /**
    * Parses a keyword as a function with two operand
    *
    * @param f function with two parameter as expression
    * @return function f's return
    */
  private def parseKeyword(f: (ExprTree, ExprTree) => ExprTree): ExprTree = {
    eat(currentToken.tokenClass)
    eat(LPAREN)
    val ret = f(parsePlusMinus, parsePlusMinus)
    eat(RPAREN)
    ret
  }

  /**
    * Parses an unary operator and an operand
    *
    * @return result of expression
    */
  private def parseUnaryOperator: ExprTree = {
    val lastToken = currentToken.tokenClass

    eat(lastToken)

    lastToken match {
      case PLUS => parseSimpleExpr
      case MINUS => Times(NumLit("-1"), parseSimpleExpr)
      case _ => expected(PLUS, MINUS)
    }
  }

}

