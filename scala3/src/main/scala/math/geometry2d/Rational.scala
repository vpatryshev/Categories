package math.geometry2d

import scala.language.{implicitConversions, postfixOps}

case class Rational(private val n0: BigInt, private val d0: BigInt = 1) extends Ordered[Rational]:
  require(d0 != 0, s"denominator can't be zero ($n0/$d0)")
  private val gcd = n0 gcd d0
  private val sigd = d0.signum
  val (n, d): (BigInt, BigInt) = if gcd == 0 then (BigInt(0), BigInt(1)) else (sigd*n0/gcd, sigd*d0/gcd)
  
  def +(other: Rational) = Rational(n * other.d + other.n * d, d * other.d)
  def -(other: Rational) = Rational(n * other.d - other.n * d, d * other.d)
  def *(other: Rational) = Rational(n * other.n, d * other.d)
  def /(other: Rational) = Rational(n * other.d, d * other.n)
  def unary_- = Rational(-n, d)
  def isPositive: Boolean = n > 0
  def isNegative: Boolean = n < 0
  def signum: Int = if isNegative then -1 else if isPositive then 1 else 0
  def abs: Rational = if isNegative then -this else this
  def isZero: Boolean = n == 0
  def toInt: Int = ((n + n.signum*d/2) / d).toInt
  def toDouble: Double = n.toDouble / d.toDouble
  
  infix def min(other: Rational): Rational = if this < other then this else other
  infix def max(other: Rational): Rational = if this < other then other else this
  
  override lazy val toString = s"$n/$d"
  
  override def equals(o: Any): Boolean = o match
    case r: Rational => n * r.d == r.n * d
    case other => false

  override def hashCode(): Int = n.hashCode() * 2017 + d.hashCode()

  override def compare(that: Rational): Int = (this - that).n compare 0

object Rational:
  given Conversion[BigInt, Rational] = Rational(_)
  given Conversion[Int,    Rational] = Rational(_)
  given Conversion[Double, Rational] = d =>
    val s = Math.signum(d).toInt
    if s == 0 then
      Rational(0)
    else
      val ndigits = Math.min(-5, Math.log10(d * s).toInt)
      val denom = Math.pow(10.0, -ndigits).toInt
      Rational((d * denom).toInt, denom)

  given Conversion[Rational, Double] = _.toDouble
