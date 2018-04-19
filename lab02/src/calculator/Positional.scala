// Laboratoire 02 - Calculatrice
//Adrien Marco, Julien Brêchet, Loan Lassalle
//we did not change anything here
package calculator

trait Positional {

  private var _pos: Int = -1

  def pos: Int = _pos

  def setPos(pos: Int): this.type = {
    this._pos = pos
    this
  }
}