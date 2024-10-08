package math.geometry2d

import math.geometry2d.Rational._
import math.geometry2d.SVG._

import scala.language.implicitConversions

case class Pt(x: Rational, y: Rational):
  def unary_- = Pt(-x, -y)
  infix def +(other: Pt): Pt = shift(other.x, other.y)
  infix def -(other: Pt): Pt = shift(-other.x, -other.y)
  infix def *(c: Rational) = Pt(c*x, c*y)
  infix def /(c: Rational) = Pt(x/c, y/c)
  def shift(dx: Rational, dy: Rational): Pt = Pt(x + dx, y + dy)
  infix def dot(other: Pt): Rational = x * other.x + y * other.y
  def scale(scale: Pt): Pt = Pt(x * scale.x, y * scale.y)
  def l2: Rational = Math.sqrt(this dot this)
  
  def asDouble: (Double, Double) = (x.toDouble, y.toDouble)

object Pt:
  def _0 = Pt(0, 0)
