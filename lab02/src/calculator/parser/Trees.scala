// Laboratoire 02 - Calculatrice
// modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator.parser

object Trees {

  sealed trait ExprTree {
    @throws(classOf[Exception])
    def compute: Double = this match {
      case _ => ???
    }
  }

  /** Nodes Expression Trees */
  /** lhs: left hand side, rhs: right hand side */
  case class Assign(ident: Identifier, value: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  ???

  /** Leaves Expression Trees */
  case class NumLit(value: String) extends ExprTree
  case class Identifier(value: String) extends ExprTree {
    override def toString: String = "Identifier('" + value + "')"
  }
}