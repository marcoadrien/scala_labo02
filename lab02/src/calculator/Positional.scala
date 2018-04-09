// Laboratoire 02 - Calculatrice
// modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

trait Positional {
  self =>

  private var _pos: Int = -1

  def pos: Int = _pos

  def setPos(pos: Int): self.type = {
    this._pos = pos
    self
  }
}