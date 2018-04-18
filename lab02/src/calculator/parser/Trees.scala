// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator.parser

import calculator.Main.memory

import scala.annotation.tailrec

object Trees {

  sealed trait ExprTree {

    /**
      * Compute an expression
      *
      * @throws java.lang.Exception is thrown if an unsupported operation is done
      * @return result of expression
      */
    @throws(classOf[Exception])
    def compute: Double = this match {
      case Plus(lhs, rhs) => lhs.compute + rhs.compute
      case Minus(lhs, rhs) => lhs.compute - rhs.compute
      case Times(lhs, rhs) => lhs.compute * rhs.compute
      case Div(lhs, rhs) =>
        val rightOperand = rhs.compute
        if (rightOperand == 0)
          throw new UnsupportedOperationException("Division by zero undefined")
        else
          lhs.compute / rightOperand
      case Mod(lhs, rhs) => lhs.compute % rhs.compute
      case Pow(lhs, rhs) => pow(lhs.compute, rhs.compute.toInt)
      case Fact(lhs) => fact(lhs.compute.toInt)
      case Assign(_, _) => Double.NegativeInfinity
      case Sqrt(rhs) =>
        val operand = rhs.compute
        if (operand < 0)
          throw new UnsupportedOperationException("Square root of a negative operand undefined")
        else
          sqrt(operand)
      case Gcd(rhs1, rhs2) => gcd(rhs1.compute.toInt, rhs2.compute.toInt)
      case ModInv(lhs, rhs) => modInv(lhs.compute.toInt, rhs.compute.toInt)
      case NumLit(value) => value.toDouble
      case Identifier(name) => getValueInMemory(name)
      case _ => throw new UnsupportedOperationException("Operation undefined")
    }
  }

  /** Nodes Expression Trees */
  /** lhs: left hand side, rhs: right hand side */
  //--------------------------------------------------------------------------------------------------------------------
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Mod(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Pow(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Fact(lhs: ExprTree) extends ExprTree

  case class Assign(ident: Identifier, value: ExprTree) extends ExprTree

  case class Sqrt(value: ExprTree) extends ExprTree

  case class Gcd(rhs1: ExprTree, rhs2: ExprTree) extends ExprTree

  case class ModInv(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  /** Leaves Expression Trees */
  case class NumLit(value: String) extends ExprTree

  case class Identifier(value: String) extends ExprTree {
    override def toString: String = s"Identifier('$value')"
  }
  //--------------------------------------------------------------------------------------------------------------------

  /** get the value of a variable in the memory
    *
    * @param ident identifier of variable
    * @return value of variable
    * @throws UnsupportedOperationException if variableName does not exist
    */
  def getValueInMemory(ident: String): Double = {
    memory.getOrElse(ident, throw new UnsupportedOperationException(s"the variable $ident does not exist"))
  }

  /** power
    *
    * @param x base
    * @param y exponent
    * @return the result of the operation
    */
  def pow(x: Double, y: Int): Double = {

    /** power
      *
      * @param x base
      * @param y exponent
      * @param acc accumulator
      * @return the result of the operation
      */
    @tailrec
    def pow(x: Double, y: Int, acc: Double): Double = y match {
      case z if z < 0 => pow(x, z + 1, acc / x)
      case 0 => acc
      case _ => pow(x, y - 1, acc * x)
    }
    pow(x, y, 1)
  }

  /** fact
    *
    * @param x factorial value
    * @return the result of the operation
    */
  def fact(x: Int): Int = {

    /** fact
      *
      * @param x factorial value
      * @param acc accumulator
      * @return the result of the operation
      */
    @tailrec
    def fact(x: Int, acc: Int): Int = if (x == 0) acc else fact(x - 1, acc * x)

    if(x < 0)
      throw new UnsupportedOperationException(s"$x is inferior to 0")
    else
      fact(x, 1)
  }

  /** greatest common divisor
    *
    * @param x first value
    * @param y second value
    * @return the result of the operation
    */
  @tailrec
  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  /** sqrt
    *
    * @param n number
    * @param epsilon precision
    * @return result of the operation
    */
  def sqrt(n: Double, epsilon: Double = 0.0001): Double = {

    /** sqrt
      *
      * @param n number
      * @param x approximation
      * @param epsilon precision
      * @return
      */
    @tailrec
    def sqrt(n: Double, x: Double, epsilon: Double): Double =
      if ((x * x - n).abs / n < epsilon)
        x
      else
        sqrt(n, (x + n / x) / 2, epsilon)
    sqrt(n, 1, epsilon)
  }

  /** modular inverse of u mod v
    *
    * @param u first value
    * @param v second value
    * @return modular inverse of u mod v
    */
  def modInv(u: Int, v: Int): Int = {
    val (x, z) = egcd(u, v)
    if (z != 1)
      throw new UnsupportedOperationException(s"$u does not have a modular inverse")
    else
      x
  }

  /** coefficients of Bézout's identity
    *
    * @param u first value
    * @param v second value
    * @return coefficients of Bézout's identity and gcd
    */
  private def egcd(u: Int, v: Int): (Int, Int) = {

    /** coefficients of Bézout's identity
      *
      * @param u first value
      * @param v second value
      * @param x coefficient of Bézout's identity
      * @param y coefficient of Bézout's identity
      * @param x1 accumulator of coefficient x
      * @param y1 accumulator of coefficient y
      * @return coefficients of Bézout's identity and gcd
      */
    @tailrec
    def egcd(u: Int, v: Int, x: Int, y: Int, x1: Int, y1: Int): (Int, Int) =
      if (v == 0)
        (x, y)
      else {
        val q = u / v
        egcd(v, u - q * v, x1, y1, x - q * x1, y - q * y1)
      }
    egcd(u, v, 1, 0, 0, 1)
  }

}
