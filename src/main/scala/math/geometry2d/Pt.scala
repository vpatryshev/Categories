package math.geometry2d
import Rational._
import SVG._

case class Pt(x: Rational, y: Rational) {
  def +(other: Pt): Pt = shift(other.x, other.y)
  def -(other: Pt): Pt = shift(-other.x, -other.y)
  def *(c: Rational) = Pt(c*x, c*y)
  def /(c: Rational) = Pt(x/c, y/c)
  def shift(dx: Rational, dy: Rational): Pt = Pt(x + dx, y + dy)
  def dot(other: Pt): Rational = x * other.x + y * other.y
  def scale(scale: Pt): Pt = Pt(x * scale.x, y * scale.y)
  def l2: Rational = Math.sqrt(this dot this)
}

object Pt {
  def _0 = Pt(0, 0)
}
