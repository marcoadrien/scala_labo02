/*
Laboratoire 02 - Calculatrice
Modifications: Adrien Marco, Julien Brêchet, Loan Lassalle
*/

package calculator.parser

import calculator.Main.memory
import calculator.lexer._
import calculator.utils.Utils

import scala.io.Source

/**
  * This class is going to parse the computation expression and compute it recursively to respect
  * the operations priority and the left associativity.
  * We respect the priority of the operations by calling each time the priority function before processing anything else.
  * For example: equals operation has less priority than Plus or Minus so we call them before. Plus or Minus have less
  * priority than Times or Division so we call them before... Finally we arrive to a simple expression as a numeric value or
  * an ID, parenthesis or even a keyword that are the most priority and we can evaluate them.
  *
  * @param source user input
  */
class Parser(source: Source) extends Lexer(source: Source) {

  import Trees._
  import calculator.lexer.Tokens._

  /** Stores the current token, as read from the lexer. */
  private var currentToken: Token = Token(BAD)

  def computeSource: Double = {
    readToken
    parseExpr.compute
  }

  def printTree: Unit = {
    readToken
    println(parseExpr)
  }

  /** Updates currentToken using nextToken in the Lexer. */
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
    * Parses an expression
    *
    * @return result of the expression
    */
  private def parseExpr: ExprTree = parseEquals

  /**
    * Parses an equal expression
    *
    * @return result of the equal expression
    */
  private def parseEquals: ExprTree = {

    // Expression analysis compared to other higher priority tokens
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
    * @return result of the addition or subtraction expression
    */
  private def parsePlusMinus: ExprTree = {

    // Expression analysis compared to other higher priority tokens
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
    * @return result of the multiplication or division expression
    */
  private def parseTimesDiv: ExprTree = {

    // Expression analysis compared to other higher priority tokens
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
    * @return result of the modular expression
    */
  private def parseMod: ExprTree = {

    // Expression analysis compared to other higher priority tokens
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
    * @return result of the exponential expression
    */
  private def parsePow: ExprTree = {

    // Expression analysis compared to other higher priority tokens
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
    * @return result of the factorial expression
    */
  private def parseFact: ExprTree = {

    // Expression analysis compared to other higher priority tokens
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
    * @return result of the simple expression
    */
  private def parseSimpleExpr: ExprTree = {
    currentToken.info match {
      case LPAREN => parseParentheses // Parentheses
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
    * @return result of the expression between parenthesis
    */
  private def parseParentheses: ExprTree = {
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(RPAREN)
    ret
  }

  /**
    * Parses a keyword as a function with one operand
    * Used to handle calculation function like square root
    *
    * @param f function with one parameter as expression
    * @return function f's return
    */
  private def parseKeyword(f: (ExprTree) => ExprTree): ExprTree = {
    eat(currentToken.tokenClass)
    f(parseParentheses)
  }

  /**
    * Parses a keyword as a function with two operand
    * Used to handle calculation function like greatest common divisor
    * or modular multiplicative inverse
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
    * Used to handle negative numbers
    *
    * @return result of the expression
    */
  private def parseUnaryOperator: ExprTree = {
    val lastToken = currentToken.tokenClass

    eat(lastToken)

    // The sign of the number is deduced from the previous token
    lastToken match {
      case PLUS => parseSimpleExpr
      case MINUS => Times(NumLit("-1"), parseSimpleExpr)
      case _ => expected(PLUS, MINUS)
    }
  }

}

