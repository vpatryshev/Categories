package xypic

import java.io.FileWriter
import java.util.Date
import scala.language.postfixOps

import math.cat.{Category, Graph}
import math.cat.Categories._
import math.geometry2d._

import scala.collection.{immutable, mutable}


case class ComponentLayout(go: GradedObjects, w: Int, h: Int) {
  val category: Category = go.category
  val base: Graph = category.baseGraph

  private var dir = (1, 0)
  private var previousLayerLength = 1
  private val taken: mutable.Set[Pt] = new mutable.HashSet[Pt]()

  private val indexedClusters: Map[Int, List[go.Cluster]] = go.layersOfClusters.zipWithIndex.map {
    case (comps, i) ⇒ i -> comps.sortBy(_.toString)
  }.toMap

  private val coordinates0: immutable.Iterable[List[(go.Cluster, Pt)]] = for {
    (i, layer) ← indexedClusters
  } yield {
    val diameters = layer.map(_.diameter)
    val layerLength = layer.length // diameters.sum
    val layerWidth = (diameters.max + 1) / 2
    val dw = layerLength - previousLayerLength
    previousLayerLength = layerLength
    val step = if (dir == (1, 0)) (layerWidth, 0) else {
      (dir._1 * layerWidth * (i % 2), dir._2 * layerWidth * ((i+1) % 2))
    }

    val row = for { (o, j) ← layer.zipWithIndex } yield {
      val newPoint = Pt(i - j + scala.math.max(0, step._1 + (dw-1)/2), step._2 + (dw-1)/2 - j)
      val actual = if (taken(newPoint)) newPoint.shift(0, -1) else newPoint
      taken.add(actual)
      o -> actual
    }
    if (layerLength > 2 * i * layerWidth + 1) dir = (layerWidth, layerWidth)
    row
  }

  def pullLeft(layers: List[List[(go.Cluster, Pt)]]): List[List[(go.Cluster, Pt)]] = {
    layers map {
      case p1::p2::tail =>
        val altPoint = p1._2 - Pt(1, 0)
        val newOne = if (!taken(altPoint)) {
          taken.remove(p1._2)
          taken.add(altPoint)
          (p1._1, altPoint)
        } else p1
        
        newOne::p2::tail
      case other => other
    }
  }

  def pullUp(layers: List[List[(go.Cluster, Pt)]]): List[List[(go.Cluster, Pt)]] = {
    layers map (_.reverse) map {
      case p1::p2::tail =>
        val altPoint = p1._2 + Pt(0, 1)
        val newOne = 
        if (!taken(altPoint)) {
          taken.remove(p1._2)
          taken.add(altPoint)
          (p1._1, altPoint)
        } else p1
        newOne::p2::tail
      case other => other
    } map (_.reverse)
  }
  
  private val coordinates1 = coordinates0.toList match {
    case layer1::tail => layer1::pullUp(pullLeft(tail))
    case Nil => Nil
  }
  
  private val coordinates: Map[String, Pt] = coordinates1.flatten.flatMap {
      case (cluster, coords) ⇒ cluster.allocateAt(coords)
  }.toMap
  
  val frame: SVG.Frame = SVG.Frame(30, Pt(w, h), coordinates.values)

  def svg: String = if (coordinates.isEmpty) "" else {
    for { (obj, p) ← coordinates } {
      frame.CircleWithText(obj.toString, p).draw()
    }

    val center = coordinates.values.reduce(_+_) / coordinates.size
    
    val arrowsWithDomainAndCodomain: Set[(base.Arrow, Set[base.Node])] =
      base.arrows.map(a ⇒ (a, Set(base.d0(a), base.d1(a))))
    
    val arrows: Map[Set[base.Node], List[base.Arrow]] =
      arrowsWithDomainAndCodomain.groupBy(_._2).
        mapValues(_.map(_._1).toList.sortBy(_.toString))
    
    def drawEndomorphisms(obj: base.Node, arrows: List[base.Arrow]): Unit = {
      val p = coordinates(obj.toString)
      for {
        (a, i) ← arrows.zipWithIndex
      } frame.Endomorphism(a.toString, p, 25, i).draw()
    }
    
    arrows.foreach {
      case (objs, arrs) if objs.size == 1 ⇒
        drawEndomorphisms(objs.head, arrs)
        
      case (objs, allArrowsBetweenTwoObjects) ⇒
        // need to take first the arrows in the direction where there are less arrows
        val arrs = allArrowsBetweenTwoObjects.groupBy(base.d0).values.toList.sortBy(_.size).flatten
        for {
          (a, i) ← arrs.zipWithIndex
        } {
          val from = coordinates(base.d0(a).toString)
          val to = coordinates(base.d1(a).toString)
          val shift0 = i
          val shift = if (i % 2 == 1) -(shift0+1)/2 else (shift0 + 1)/2
          frame.Arrow(a.toString, Segment(from, to), 25, shift, center).draw()
        }
    }

    frame.toString
  }
}

case class Layout(category: Category, w: Int, h: Int) {
  
  val gradedObjects: Set[GradedObjects] = category.connectedComponents map GradedObjects
  
  def wideDigit(c: Char): String = s"&#${c - '0' + 0x1D7D8};"
  
  private val digits = "_(\\d+)_".r
  private val withIndex = "([^_]+)_?(\\d+)".r
  
  val name: String = category.name match {
    case digits(n) ⇒ n map wideDigit mkString
    case withIndex(word, n) ⇒ s"$word<sub>$n</sub>"
    case other ⇒ other
  }
  
  private val layouts = gradedObjects map (ComponentLayout(_, w, h))
  private val htmls = layouts map (_.svg)
  val html: String = htmls.mkString(
    s"<table><tr><th colspan=${htmls.size}><font size=+1>$name</font></th></tr>" +
    s"<tr><td><font size=-1>$category</font></td></tr>" +
    "<tr><td>",
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
    val frame = SVG.Frame(15, Pt(w, h))
    val c = Pt(w/2, h/2)
    val withCoords: Set[(Any, Pt)] = GroupOfObjects(seq).arrangeInCircle(c, 100)

    withCoords foreach {
      case (name, p) ⇒ frame.CircleWithText(name.toString, p).draw()
    }
    
    "<br/>" + frame
  }

  val out = new FileWriter("samples.html")

  def main(args: Array[String]): Unit = {
    writeHtml(Layout(Simplicial3, 300, 300).html)

    showAll()

    out.close()
    println(s"Done: ${new Date}")
    
  }
  
  private def showAll(): Unit = {
    val fullMap = for {
      c ← KnownFiniteCategories
      layout = Layout(c, 300, 300)
      html = layout.html
    } yield html

    val htmlVersion = fullMap mkString "<hr/>"
    writeHtml(htmlVersion)

    writeHtml(polygon('a' until 'j'))
  }

  def writeHtml(content: String): Unit = {
    out.write(html(content))
  }
}