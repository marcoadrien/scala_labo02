// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

trait Positional {

  private var _pos: Int = -1

  def pos: Int = _pos

  def setPos(pos: Int): this.type = {
    this._pos = pos
    this
  }
}