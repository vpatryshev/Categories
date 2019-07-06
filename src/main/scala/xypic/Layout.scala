package xypic

import java.io.FileWriter

import math.cat.{Category, Graph}
import math.cat.Categories._
import math.geometry2d
import math.geometry2d.{Pt, Rational, Segment}

import scala.collection.mutable



case class ComponentLayout(go: GradedObjects, w: Int, h: Int) {
  val category = go.category
  val base = category.baseGraph
  val name: String = category.name
  private val indexed = go.layers.zipWithIndex.map { case (s, i) => i -> s.toList.sortBy(_.toString)}.toMap
  private var dir = (1, 0)
  private var prevW = 1
  private val taken: mutable.Set[Pt] = new mutable.HashSet[Pt]()

  private val coordinates0 = for {
    (i, os) <- indexed
  } yield {
    val w = os.length
    val dw = w - prevW
    prevW = w
    val step = if (dir == (1, 0)) dir else {
      (dir._1 * (i % 2), dir._2*((i+1) % 2))
    }

    val row = for { (o, j) <- os.zipWithIndex } yield {
      val newPoint = Pt(i - j + scala.math.max(0, step._1 + (dw-1)/2), step._2 + (dw-1)/2 - j)
      val actual = if (taken(newPoint)) newPoint.shift(0, -1) else newPoint
      taken.add(actual)
      o -> actual
    }
    if (w > 2 * i + 1) dir = (1, 1)
    row
  }

  private val coordinates1: Map[String, Pt] = coordinates0.flatten.toMap.map {
      case (k, v) => k.toString -> v
  }
  
  private val xys0 = coordinates1.values.toList
  private val xys = xys0.headOption.getOrElse(Pt(0, 0)) :: xys0
  private val x0 = xys.minBy(_.x).x
  private val x1 = xys.maxBy(_.x).x
  private val y0 = xys.minBy(_.y).y
  private val y1 = xys.maxBy(_.y).y
  val p0 = Pt(x0, y0)
  private val middle = Pt((x0+x1)/2, (y0+y1)/2)
  val s = Pt(w / (x1-x0+2), -h / (y1-y0+2))

  val coordinates: Map[String, Pt] = coordinates1 mapValues {
    p => (p - middle).scale(s) + Pt(w/2, h/2)
  }
  
  def draw[T](renderer: (String, (Double, Double)) => T): Iterable[T] = {
    
    for {
      (obj, p) <- coordinates
    } yield {
      renderer(obj.toString, p.toDouble)
    }
  }

  def svg: String = {

    val objects = draw((txt, p) =>
      s"""
         |<text x="${p._1-4}" y=${p._2+4}>$txt</text>
         |<circle cx="${p._1}" cy="${p._2}" r="10"
         | style="fill:yellow;stroke:black;stroke-width:2;opacity:0.3" />
         |""".stripMargin
    )
    
    val arrowMap: Map[String, Segment] = base.arrows.map(a => a.toString -> {
      val from = coordinates(base.d0(a).toString)
      val to = coordinates(base.d1(a).toString)
      Segment(from, to)
    }
    ).toMap
    
    def drawArrow(seg: Segment): String = {
      val q = 10.0
      val shorter = seg.shorterBy(25)
      val p0 = shorter.p0.toDouble
      val p1 = shorter.p1.toDouble
      if (p0 != p1) {
        val ortho = (p1._2 - p0._2, p0._1 - p1._1)
        val norm = Math.sqrt(ortho._1*ortho._1 + ortho._2*ortho._2)
        val ortho1 = (ortho._1 / norm, ortho._2 / norm)
        val arrowhead = (p1._1 + (p0._1 - p1._1)/norm * q, p1._2 + (p0._2 - p1._2)/norm * q)
        val ap0 = (arrowhead._1 - ortho1._1 * q/3, arrowhead._2 - ortho1._2 * q/3)
        val ap1 = (arrowhead._1 + ortho1._1 * q/3, arrowhead._2 + ortho1._2 * q/3)
        s"""<line x1="${p0._1}" y1="${p0._2}" x2="${p1._1}" y2="${p1._2}"
           | style="stroke:darkgray;stroke-width:1" />
           | <polyline points="${ap0._1},${ap0._2} ${p1._1},${p1._2} ${ap1._1},${ap1._2}"
           | style="fill:none;stroke:darkgray;stroke-width:1" />
           |""".stripMargin
      }
      else
        s"""<line x1="${p0._1}" y1="${p0._2}" x2="${p1._1}" y2="${p1._2}"
           | style="stroke:darkgray;stroke-width:1" />
           |""".stripMargin
    }
    
    val arrows: Iterable[String] = arrowMap.values map drawArrow
    
    s"""<svg width="$w" height="$h">
       |  <rect x="0" y="0" rx="15" ry="15" width="$w" height="$h"
       |  style="fill:blue;stroke:black;stroke-width:5;opacity:0.03" />
       |
       |${objects mkString "\n"}
       |${arrows mkString "\n"}
       |</svg>
       |""".stripMargin
  }
  
  def html(name: String): String = s"<h3>$name</h3>$svg"
}

case class Layout(category: Category, w: Int, h: Int) {
  
  val gradedObjects: Set[GradedObjects] = category.connectedComponents map GradedObjects
  
  def wideDigit(c: Char): String = s"&#${c - '0' + 0x1D7D8};"
  
  private val digits = "_(\\d+)_".r
  private val withIndex = "([^_]+)_?(\\d)".r
  
  val name = category.name match {
    case digits(n) => n map wideDigit mkString
    case withIndex(w, n) => s"$w<sub>$n</sub>"
    case other => other
  }
  
  private val layouts = gradedObjects map (ComponentLayout(_, w, h))
  private val htmls = layouts map (_.svg)
  val html = htmls.mkString(
    s"<table><tr><th colspan=${htmls.size}><font size=+1>$name</font></th></tr><tr><td>",
    "</td><td>",
    "</td></tr></table>\n")
}

object TestIt {
  
  def html(content: String): String =
    s"""
       |<html>
       |<head>
       |<meta http-equiv="refresh" content="40">
       |</head>
       |<body>
       |$content
       |</body>
       |</html>
     """.stripMargin
  
  def main(args: Array[String]): Unit = {
    val fullMap = for {
      c <- KnownFiniteCategories
      layout = Layout(c, 300, 300)
      html = layout.html
    } yield html
    
    val htmlVersion = fullMap mkString "<hr/><p/>"

    writeHtml(htmlVersion)
    val repr = fullMap.mkString("Map(\n  ", ",\n  ", "\n)")
  }
  
  def writeHtml(content: String): Unit = {
    val out = new FileWriter("cats.html")
    out.write(html(content))
    out.close
  }
}