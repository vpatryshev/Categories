package math.geometry2d

object SVG {

  def polyline(points: Pt*): String = {
    points map ((p:Pt) => s"${p.x.toDouble},${p.y.toDouble}") mkString(
      """<polyline points="""",
      " ",
      """style="fill:none;stroke:black;stroke-width:1" />""")
  }
  def apply(p: Pt): String = s"${p.x.toDouble} ${p.y.toDouble}"
  
  def apply(seg: Segment): String = {
    val p0 = seg.p0
    val p1 = seg.p1
    s"""<line x1="${p0.x.toDouble}" y1="${p0.y.toDouble}" x2="${p1.x.toDouble}" y2="${p1.y.toDouble}"
       | style="stroke:black;stroke-width:1" />
     """.stripMargin
  }

  def arrowhead(seg: Segment): String = {
    val q = 7
    val headStart = seg.p1 - (seg.unit * q)
    SVG.polyline(headStart - seg.orthonorm*q/3, seg.p1, headStart + seg.orthonorm*q/3)
  }

  def arrow(seg: Segment): String = apply(seg) + arrowhead(seg)

}
