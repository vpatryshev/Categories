package math.geometry2d

import math.geometry2d.SVG.Frame

import scala.collection.mutable.ArrayBuffer

object SVG {

  def polyline(points: Pt*): String = {
    points map ((p:Pt) => s"${p.x.toDouble},${p.y.toDouble}") mkString(
      """<polyline points="""",
      " ",
      """style="fill:none;stroke:black;stroke-width:1" />""")
  }
  def point(p: Pt): String = s"${p.x.toDouble} ${p.y.toDouble}"
  
  def segment(seg: Segment, color: String = "black"): String = {
    val p0 = seg.p0
    val p1 = seg.p1
    s"""<line x1="${p0.x.toDouble}" y1="${p0.y.toDouble}" x2="${p1.x.toDouble}" y2="${p1.y.toDouble}"
       | style="stroke:$color;stroke-width:1" />
     """.stripMargin
  }

  private def arrowhead(seg: Segment): String = {
    val q = 7
    val headStart = seg.p1 - (seg.unit * q)
    SVG.polyline(headStart - seg.orthonorm*q/3, seg.p1, headStart + seg.orthonorm*q/3)
  }
  
  case class Frame(w: Int, h: Int, grid: Int, topLeft: Pt, bottomRight: Pt) { frame =>
    private val dx = w/grid
    private val dy = h/grid
    val center: Pt = (topLeft + bottomRight) / 2
    private val origSize = bottomRight - topLeft + Pt(2, 2)
    val scale = snap(Pt((w + origSize.x/2) / origSize.x, (-h - origSize.y/2) / origSize.y))

    def snap(x: Rational) = Rational(((x*2+1)/2/grid).toInt*grid, 1)
    
    def snap(pt: Pt): Pt = Pt(snap(pt.x), snap(pt.y))

    def rescale(p: Pt): Pt = {
      val relativeToCenter = (p - center).scale(scale)
      val absolute = relativeToCenter + Pt(w / 2, h / 2)
      val p1 = snap(absolute)
      p1
    }

    def rescale(seg: Segment): Segment = Segment(rescale(seg.p0), rescale(seg.p1))
    
    private val buffer: ArrayBuffer[String] = new ArrayBuffer
    def draw(contents: Shape*): Unit = buffer.append(contents mkString "\n")
    
    def gr(): Unit = {
      for {
        i <- 1 to grid - 1
      } {
        buffer.append(segment(Segment(Pt(dx * i, 0), Pt(dx * i, w)), "lightgray"))
        buffer.append(segment(Segment(Pt(0, dy * i), Pt(h, dy * i)), "lightgray"))
      }
    }
    
    gr()
    
    override def toString: String =
      s"""<svg width="$w" height="$h">
         |  <rect x="0" y="0" rx="$dx" ry="$dy" width="$w" height="$h"
         |  style="fill:blue;stroke:black;stroke-width:5;opacity:0.03" />
         |
         |${buffer mkString "\n"}
         |</svg>
         |""".stripMargin

    trait Shape {
      def draw(): Unit = frame.draw(this)
    }

    case class CircleWithText(txt: String, center: Pt) extends Shape {

      override def toString: String = {
        val localCenter = frame.rescale(center)

        val tp = localCenter + Pt(-4, 4)
        s"""
           |<text ${tp.svgWithPrefix("")}>$txt</text>
           |<circle ${localCenter.svgWithPrefix("c")} r="10"
           | style="fill:yellow;stroke:black;stroke-width:2;opacity:0.3" />
           |""".stripMargin
      }
    }

    case class Arrow(fullSegment: Segment, space: Int) extends Shape {
      override def toString: String = {
        val localSegment = frame.rescale(fullSegment)
        if (localSegment.isPoint) {
          val sp0 = localSegment.p0
          val s00 = Segment(sp0, sp0 + Pt(-70, -70))
          val s0 = s00.shorterBy(space)
          val sp1 = localSegment.p1
          val s1 = Segment(localSegment.p1 + Pt(70, -70), sp1).shorterBy(space)
          s"""
             |<path d="M ${point(s0.p0)} C ${point(s0.p1)}, ${point(s1.p0)}, ${point(s1.p1)}"
             | stroke="black" fill="transparent"/>
             | ${SVG.arrowhead(Segment(s1.p0, s1.p1))}
         """.stripMargin
        } else {
          val shortSegment = localSegment.shorterBy(space)
          segment(shortSegment) + arrowhead(shortSegment)
        }
      }
    }
  }

  def Frame(diagonal: Pt, points: Iterable[Pt], raster: Int): Frame = {
    val xys = points ++ (points.headOption.getOrElse(Pt(0, 0))::Nil)

    val p0 = Pt(xys.minBy(_.x).x, xys.minBy(_.y).y)
    val p1 = Pt(xys.maxBy(_.x).x, xys.maxBy(_.y).y)
    Frame(diagonal.x.toInt, diagonal.y.toInt, raster, p0, p1)
  }

}
