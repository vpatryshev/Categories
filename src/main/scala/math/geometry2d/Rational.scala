package math.geometry2d

case class Rational(private val n0: BigInt, private val d0: BigInt = 1) extends Ordered[Rational] { R =>
  require(d0 != 0)
  private val gcd = n0 gcd d0
  private val sig = d0.signum
  val n: BigInt = if (gcd == 0) 0 else sig*n0/gcd
  val d: BigInt = sig*d0/gcd
  
  def +(other: Rational) = Rational(n * other.d + other.n * d, d * other.d)
  def -(other: Rational) = Rational(n * other.d - other.n * d, d * other.d)
  def *(other: Rational) = Rational(n * other.n, d * other.d)
  def /(other: Rational) = Rational(n * other.d, d * other.n)
  def unary_- = Rational(-n, d)
  def isPositive: Boolean = n > 0
  def isNegative: Boolean = n < 0
  def isZero: Boolean = n == 0
  def toInt: Int = ((n + n.signum*d/2) / d).toInt
  def toDouble: Double = n.toDouble / d.toDouble
  
  def min(other: Rational): Rational = if (this < other) this else other
  def max(other: Rational): Rational = if (this < other) other else this
  
  override def toString = s"$n/$d"
  
  override def equals(o: Any): Boolean = o match {
    case r: Rational => n * r.d == r.n * d
    case other => false
  }

  override def hashCode(): Int = n.hashCode() * 2017 + d.hashCode()

  override def compare(that: Rational): Int =
    if (that == this) 0
    else if((this - that).isPositive) 1
    else -1
}

object Rational {
  implicit def fromBigInt(n: BigInt): Rational = Rational(n)
  implicit def fromInt(n: Int): Rational = Rational(n)
}
