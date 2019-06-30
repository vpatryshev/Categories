package math.geometry2d

case class Segment(p0: Pt, p1: Pt) {
  private val minx = p0.x min p1.x
  private val maxx = p0.x max p1.x
  private val miny = p0.y min p1.y
  private val maxy = p0.y max p1.y
  private val dp = p1 - p0
  private val orthogonal = Pt(dp.y, -dp.x)
  
  def contains(p: Pt): Boolean = {
    val d = p - p0
    val online = d.x * dp.y == d.y * dp.x
    def between: Boolean = p.x >= minx && p.x <= maxx && p.y >= miny && p.y <= maxy
    online && between
  }
  
  def shift(vector: Pt) = Segment(p0 + vector, p1 + vector)
  
  private def intersectsLine(seg: Segment): Boolean = {
    val otherP0 = seg.p0
    val otherP1 = seg.p1
    (otherP0 dot orthogonal) * (otherP1 dot orthogonal) <= 0
  }
  
  def interesectsWith(other: Segment): Boolean =
    intersectsLine(other) && other.intersectsLine(this)
}
