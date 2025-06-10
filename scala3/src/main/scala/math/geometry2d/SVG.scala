package math.geometry2d

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object SVG:

  private def polyline(points: Pt*): String =
    points map point mkString(
      """<polyline points="""",
      " ",
      """" style="fill:none;stroke:black;stroke-width:1" />""")
    
  def point(p: Pt): String = s"${p.x.toDouble} ${p.y.toDouble}"
  
  def segment(seg: Segment, color: String = "black"): String =
    val p0 = seg.p0
    val p1 = seg.p1
    s"""<line x1="${p0.x.toDouble}" y1="${p0.y.toDouble}" x2="${p1.x.toDouble}" y2="${p1.y.toDouble}"
       | style="stroke:$color;stroke-width:1" />
     """.stripMargin

  private def arrowhead(seg: Segment): String =
    val q = 7
    val headStart = seg.p1 - (seg.unit * q)
    SVG.polyline(headStart - seg.orthonorm*q/3, seg.p1, headStart + seg.orthonorm*q/3)
  
  case class Frame(diagonal: Pt, grid: Int, range: Segment): 
    frame =>
    val w: Int = diagonal.x.toInt
    val h: Int = diagonal.y.toInt
    private val dx = w/grid
    private val dy = h/grid
    private val topLeft: Pt = range.p0
    private val bottomRight: Pt = range.p1
    private val center: Pt = (topLeft + bottomRight) / 2
    private val origSize = bottomRight - topLeft + Pt(2, 2)
    private val scale =
      val raw = Pt(Rational(w) / origSize.x, Rational(-h) / origSize.y)
      val snapped = snap(raw)
      Pt(
        (raw.x.abs max snapped.x) * raw.x.signum,
        (raw.y.abs max snapped.y) * raw.y.signum)

    private def snap(x: Rational) = Rational((x/grid).toInt*grid)
    
    private def snap(pt: Pt): Pt = Pt(snap(pt.x), snap(pt.y))

    private def toLocal(p: Pt): Pt =
      val relative = p - center
      val relativeScaled = relative.scale(scale)
      relativeScaled + Pt(w / 2, h / 2)
    
    private def rescale(p: Pt): Pt = snap(toLocal(p))

    private def rescale(seg: Segment): Segment = Segment(rescale(seg.p0), rescale(seg.p1))
    
    private val buffer: ArrayBuffer[String] = new ArrayBuffer
    def draw(contents: Shape*): Unit = buffer.append(contents mkString "\n")

    for i <- 1 to grid do
      buffer.append(segment(Segment(Pt(dx * i, 0), Pt(dx * i, w)), "lightgray"))
      buffer.append(segment(Segment(Pt(0, dy * i), Pt(h, dy * i)), "lightgray"))
    
    override lazy val toString: String =
      s"""<svg width="$w" height="$h">
         |  <rect x="0" y="0" rx="$dx" ry="$dy" width="$w" height="$h"
         |  style="fill:blue;stroke:black;stroke-width:5;opacity:0.03" />
         |
         |${buffer mkString "\n"}
         |</svg>
         |""".stripMargin

    trait Shape:
      def draw(): Unit = frame.draw(this)
      
      def text(txt: String)(p: Pt): String =
        s"""<text text-anchor="middle" style="font: 14px sans-serif;" dominant-baseline="central"
           |${asSvg(p)}>$txt</text>""".stripMargin

    private def asSvg(p: Pt, prefix: String = ""): String =
      s"""${prefix}x="${p.x.toDouble}" ${prefix}y="${p.y.toDouble}""""

    case class CircleWithText(txt: String, cc: Pt) extends Shape:

      override lazy val toString: String =
        val localCenter = frame.rescale(cc)
        text(txt)(localCenter) + s"""
           |<circle ${asSvg(localCenter, "c")} r="10"
           | style="fill:yellow;stroke:black;stroke-width:2;opacity:0.3" />
           |""".stripMargin

    // see also https://developer.mozilla.org/en-US/docs/Web/SVG/Element/textPath
    
    case class Endomorphism(name: String, p0: Pt, space: Int, ord: Int) extends Shape:
      override lazy val toString: String =
        val p = frame.rescale(p0)
        val curve = 45 * (ord + 1)
        val s00 = Segment(p, p + Pt(-curve, -curve))
        val s0 = s00.shorterBy(space)
        val s1 = Segment(p + Pt(curve, -curve), p).shorterBy(space)
        val id = s"endo_$name"
        val out = text(name)(p+Pt(0, -curve*3/4 - 5)) + 
          s"""
        |<path id="$id" d="M ${point(s0.p0)} C ${point(s0.p1)}, ${point(s1.p0)}, ${point(s1.p1)}"
        | stroke="black" fill="none"/>""".stripMargin +
          arrowhead(Segment(s1.p0, s1.p1))
        out
    
    case class Arrow(
      name: String,
      fullSegment: Segment,
      space: Int,
      ord: Int,
      center: Pt
    ) extends Shape:
      override lazy val toString: String =
        val localSegment = frame.rescale(fullSegment)
        if ord == 0 then
          val localCenter = rescale(center)
          val shortSegment = localSegment.shorterBy(space)
          val middle = localSegment.middle
          val drawTextAbove = ((middle - localCenter) dot localSegment.orthogonalUnit) < 0
          val unit = if drawTextAbove then -localSegment.orthogonalUnit else localSegment.orthogonalUnit
          val dx = unit.x * name.length * 5

          val shift = Pt(dx, unit.y * 8)
          text(name)(localSegment.middle + shift) +
          segment(shortSegment) + arrowhead(shortSegment)
        else
          val p0 = localSegment.p0
          val p1 = localSegment.p1
          val dx = Math.signum(p1.x - p0.x)
          val curve = 50 * ord
          val s00 = Segment(p0, p0 + Pt(dx*Math.abs(curve), -curve/2))
          val s0 = s00.shorterBy(space)
          val s01 = Segment(p1 + Pt(-dx*Math.abs(curve), -curve/2), p1)
          val s1 = s01.shorterBy(space)
          val midpoint = (s00.p1 + s01.p0) / 2
          val out = text(name)(midpoint) +
            s"""<path d="M ${point(s0.p0)} C ${point(s0.p1)}, ${point(s1.p0)}, ${point(s1.p1)}"
            | stroke="black" fill="transparent"/>""".stripMargin +
            arrowhead(Segment(s1.p0, s1.p1))
          out

  def Frame(raster: Int, diagonal: Pt, points: Iterable[Pt] = Nil): Frame =
    val range = points match
      case Nil => Segment(Pt(0, 0), diagonal)
      case other =>
        val xs = points.map(_.x)
        val ys = points.map(_.y)
        Segment(Pt(xs.min, ys.min), Pt(xs.max, ys.max))

    Frame(diagonal, raster, range)

