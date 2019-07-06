package math.geometry2d

case class Segment(p0: Pt, p1: Pt) {
  private val minx = p0.x min p1.x
  private val maxx = p0.x max p1.x
  private val miny = p0.y min p1.y
  private val maxy = p0.y max p1.y
  private val dp = p1 - p0
  val orthogonal = Pt(dp.y, -dp.x)
  val middle: Pt = p0 + (dp / 2)
  val isPoint: Boolean = p0 == p1
  val l2: Rational = dp.l2
  lazy val unit: Pt = dp / l2
  lazy val orthonorm: Pt = orthogonal / l2
  
  def contains(p: Pt): Boolean = {
    val d = p - p0
    val online = d.x * dp.y == d.y * dp.x
    def between: Boolean = p.x >= minx && p.x <= maxx && p.y >= miny && p.y <= maxy
    online && between
  }
  
  def shift(vector: Pt) = Segment(p0 + vector, p1 + vector)
  
  private[geometry2d] def intersectsLine(seg: Segment): Boolean = {
    val otherP0 = seg.p0 - p0
    val otherP1 = seg.p1 - p0
    (otherP0 dot orthogonal) * (otherP1 dot orthogonal) <= 0
  }
  
  def interesectsWith(other: Segment): Boolean =
    intersectsLine(other) && other.intersectsLine(this)

  def shorterBy(delta: Rational): Segment = {
    val length = dp.l2
    if (length.isZero) this else {
      val q = (length - delta) / length
      shorten(q)
    }
  }
  
  def shorten(coeff: Rational) = Segment(middle - (dp / 2 * coeff), middle + (dp / 2 * coeff))

}
