/*
Laboratoire 02 - Calculatrice
Modifications: Adrien Marco, Julien Brêchet, Loan Lassalle
*/

package calculator.parser

import calculator.Main.memory

import scala.annotation.tailrec

/**
  * This class defines a calculation tree with nodes (operations) and leaves (values).
  * The operations are implemented here so we can compute each sub tree of the tree to get a final result.
  * Leaves are computed at the beginning of node operations like Plus, Minus, Times.
  * If we represent the calculation with a tree, we start by the evaluation of the leaves to use them as parameter in the
  * node operation to evaluate the new value and we do it again for the next level of the tree (recursion)
  */
object Trees {

  sealed trait ExprTree {

    /**
      * Computes recursively an expression Tree by matching operations depending on the nodes
      * Double.NegativeInfinity is used to inform the upper layer that the memory has been updated
      *
      * @throws java.lang.UnsupportedOperationException is thrown if an unsupported operation is done
      * @return result of the expression
      */
    @throws(classOf[UnsupportedOperationException])
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
      case Mod(lhs, rhs) =>
        val rightOperand = rhs.compute
        if (rightOperand == 0)
          throw new UnsupportedOperationException("Modulo by zero undefined")
        else
          lhs.compute % rightOperand
      case Pow(lhs, rhs) => pow(lhs.compute, rhs.compute.toInt)
      case Fact(lhs) => fact(lhs.compute.toInt)
      case Assign(_, _) => Double.NegativeInfinity // Memory updated case
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
      case _ => throw new UnsupportedOperationException("Operation unsupported")
    }
  }

  /** Nodes Expression Trees */
  /** lhs: left hand side, rhs: right hand side */
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

  /** Gets the value of a variable in the memory
    *
    * @param ident identifier of variable
    * @return value of variable
    * @throws UnsupportedOperationException if identifier of variable does not exist
    */
  def getValueInMemory(ident: String): Double = {
    memory.getOrElse(ident, throw new UnsupportedOperationException(s"the variable $ident does not exist"))
  }

  /** Exponentiation
    *
    * @param x base
    * @param y exponent
    * @return x exponent y
    */
  def pow(x: Double, y: Int): Double = {

    /** Exponentiation tail recursive
      *
      * @param x base
      * @param y exponent
      * @param acc accumulator
      * @return x exponent y
      */
    @tailrec
    def pow(x: Double, y: Int, acc: Double): Double = y match {
      case z if z < 0 => pow(x, z + 1, acc / x)
      case 0 => acc
      case _ => pow(x, y - 1, acc * x)
    }
    pow(x, y, 1)
  }

  /** Factorial
    *
    * @param x factorial value
    * @return factorial of x
    */
  def fact(x: Int): Int = {

    /** Factorial tail recursive
      *
      * @param x factorial value
      * @param acc accumulator
      * @return factorial of x
      */
    @tailrec
    def fact(x: Int, acc: Int): Int = if (x == 0) acc else fact(x - 1, acc * x)

    if(x < 0)
      throw new UnsupportedOperationException(s"$x is inferior to 0")
    else
      fact(x, 1)
  }

  /** Greatest common divisor
    *
    * @param x first value
    * @param y second value
    * @return the result of the operation
    */
  @tailrec
  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  /** Square root
    *
    * @param n number
    * @param epsilon precision
    * @return square root of n
    */
  def sqrt(n: Double, epsilon: Double = 0.0001): Double = {

    /** Square root tail recursive
      *
      * @param n number
      * @param x approximation
      * @param epsilon precision
      * @return square root of n
      */
    @tailrec
    def sqrt(n: Double, x: Double, epsilon: Double): Double =
      if ((x * x - n).abs / n < epsilon)
        x
      else
        sqrt(n, (x + n / x) / 2, epsilon)
    if (n == 0) n else sqrt(n, 1, epsilon)
  }

  /** Modular multiplicative inverse
    *
    * @param a first value
    * @param m second value
    * @return modular multiplicative inverse of a modulo m
    */
  def modInv(a: Int, m: Int): Int = {
    val (x, y, _) = egcd(a, m)
    if (x != 1)
      throw new UnsupportedOperationException(s"$a does not have a modular multiplicative inverse")
    else if (y < 0)
      m + y
    else
      y
  }

  /** Extended Euclidean
    *
    * @param a first value
    * @param b second value
    * @return greatest common divisor and coefficients of Bézout's identity
    */
  def egcd(a: Int, b: Int): (Int, Int, Int) = {

    /** Extended Euclidean tail recursive
      *
      * @param r first value
      * @param u coefficient of Bézout's identity
      * @param v coefficient of Bézout's identity
      * @param r1 second value
      * @param u1 accumulator of u coefficient
      * @param v1 accumulator of y coefficient
      * @return greatest common divisor and coefficients of Bézout's identity
      */
    @tailrec
    def egcd(r: Int, u: Int, v: Int, r1: Int, u1: Int, v1: Int): (Int, Int, Int) =
      if (r1 == 0)
        (r, u, v)
      else {
        val q = r / r1
        egcd(r1, u1, v1, r - q * r1, u - q * u1, v - q * v1)
      }
    egcd(a, 1, 0, b, 0, 1)
  }

}
