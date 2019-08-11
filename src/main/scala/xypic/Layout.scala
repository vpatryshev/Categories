package xypic

import java.io.FileWriter
import java.util.Date

import math.cat.{Category, Graph}
import math.cat.Categories._
import math.geometry2d._

import scala.collection.mutable


case class ComponentLayout(go: GradedObjects, w: Int, h: Int) {
  val category: Category = go.category
  val base: Graph = category.baseGraph

  private var dir = (1, 0)
  private var previousLayerLength = 1
  private val taken: mutable.Set[Pt] = new mutable.HashSet[Pt]()

  private val indexedClusters: Map[Int, List[go.Cluster]] = go.layersOfClusters.zipWithIndex.map {
    case (comps, i) => i -> comps.toList.sortBy(_.toString)
  }.toMap

  private val coordinates0 = for {
    (i, layer) <- indexedClusters
  } yield {
    val diameters = layer.map(_.diameter)
    val layerLength = layer.length // diameters.sum
    val layerWidth = (diameters.max + 1) / 2
    val dw = layerLength - previousLayerLength
    previousLayerLength = layerLength
    val step = if (dir == (1, 0)) (layerWidth, 0) else {
      (dir._1 * layerWidth * (i % 2), dir._2 * layerWidth * ((i+1) % 2))
    }

    val row = for { (o, j) <- layer.zipWithIndex } yield {
      val newPoint = Pt(i - j + scala.math.max(0, step._1 + (dw-1)/2), step._2 + (dw-1)/2 - j)
      val actual = if (taken(newPoint)) newPoint.shift(0, -1) else newPoint
      taken.add(actual)
      o -> actual
    }
    if (layerLength > 2 * i * layerWidth + 1) dir = (layerWidth, layerWidth)
    row
  }

  private val coordinates: Map[String, Pt] = coordinates0.flatten.map {
      case (cluster, coords) => cluster.objects.map(obj => obj.toString -> coords)
  }.flatten.toMap
  
  val frame = SVG.Frame(Pt(w, h), coordinates.values, 30)

  def svg: String = {
    for { (obj, p) <- coordinates } frame.CircleWithText(obj.toString, p).draw()
    
    val arrowMap: Map[String, Unit] = base.arrows.map(a => a.toString -> {
      val from = coordinates(base.d0(a).toString)
      val to = coordinates(base.d1(a).toString)
      frame.Arrow(Segment(from, to), 25).draw()
    }
    ).toMap
    
//    def drawArrow(seg: Segment): Unit = frame.Arrow(seg, 25).draw()

//    arrowMap.values foreach drawArrow
    frame.toString
  }
}

case class Layout(category: Category, w: Int, h: Int) {
  
  val gradedObjects: Set[GradedObjects] = category.connectedComponents map GradedObjects
  
  def wideDigit(c: Char): String = s"&#${c - '0' + 0x1D7D8};"
  
  private val digits = "_(\\d+)_".r
  private val withIndex = "([^_]+)_?(\\d+)".r
  
  val name: String = category.name match {
    case digits(n) => n map wideDigit mkString
    case withIndex(word, n) => s"$word<sub>$n</sub>"
    case other => other
  }
  
  private val layouts = gradedObjects map (ComponentLayout(_, w, h))
  private val htmls = layouts map (_.svg)
  val html: String = htmls.mkString(
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
       |<h3>${new Date}</h3>
       |$content
       |</body>
       |</html>
     """.stripMargin

  def polygon(seq: Seq[Any]): String = {
    val w = 300
    val h = 300
    val frame = SVG.Frame(w, h, 15, Pt(0, 0), Pt(w, h))
    val size = seq.size
    val da = 2 * Math.PI / size
    val step = 30 * 1.4142135
    val r = step / 2 / Math.sin(da/2)
    val c = Pt(w/2, step + Rational.fromDouble(r)) 
    def p(i: Int): Pt = {
      val a = da * i + Math.PI/6
      val x = -r * Math.cos(a)
      val y = r * Math.sin(a)
      c + Pt(Rational.fromDouble(x), Rational.fromDouble(y))
    }
    for { (n, i) <- seq.zipWithIndex} {
      frame.CircleWithText(n.toString, p(i)).draw()
    }
    
    "<br/>" + frame
  }
  
  
  
  def main(args: Array[String]): Unit = {
    writeHtml(Layout(_3_, 300, 300).html)
    writeHtml(Layout(AAAAAA, 300, 300).html)

    showAll()
    println(s"Done: ${new Date}")
  }

  private def showAll() = {
    val fullMap = for {
      c <- KnownFiniteCategories
      layout = Layout(c, 300, 300)
      html = layout.html
    } yield html

    val htmlVersion = fullMap mkString "<hr/><p/>"

    writeHtml(htmlVersion + polygon('a' until 'g'))
  }

  def writeHtml(content: String): Unit = {
    val out = new FileWriter("cats.html")
    out.write(html(content))
    out.close()
  }
}