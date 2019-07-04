package xypic

import java.io.FileWriter

import math.cat.{Category, Graph}
import math.cat.Categories._
import math.geometry2d.Pt

import scala.collection.mutable





case class GradedObjects(category: Category) {
  val allArrows: Set[category.Arrow] = category.arrows
  
  def group(arrows: Set[category.Arrow]): Map[Int, Set[category.Node]] = {
    val objWithSizes = arrows.groupBy(category.d1).mapValues(_.size)
    val groupedBySize = objWithSizes.groupBy(_._2).mapValues(_.keySet)
    groupedBySize
  }
  
  def head(arrows: Set[category.Arrow]): Set[category.Node] =
    group(arrows).toList.sortBy(_._1).headOption.map(_._2).toSet.flatten
  
  def arrowsNotTo(objs: Set[category.Node]): Set[category.Arrow] = category.arrows.filterNot(a => objs(category.d1(a)))

  def arrowsNotConnecting(objs: Set[category.Node]): Set[category.Arrow] = category.arrows.filterNot(a => objs(category.d1(a)) || objs(category.d0(a)))
  
  def arrowsFrom(o: category.Node): Set[category.Arrow] =
    allArrows.filter(a => category.d0(a) == o && category.d1(a) != o)
  
  def next(objs: Set[category.Node] = Set.empty): (Set[category.Node], Set[category.Node]) = {
    val newOne = head(arrowsNotConnecting(objs))
    (objs union newOne, newOne)
  }
  
  lazy val layers: List[Set[category.Node]] = {
    Stream.iterate(next(Set.empty[category.Node])){
      case (sum, current) =>
        val (ns, nc) = next(sum)
        (ns, nc)
    } takeWhile(_._2.nonEmpty)
  } map (_._2) toList
}

case class Layout1(go: GradedObjects) {
  val name: String = go.category.name
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

  val coordinates: Map[String, Pt] = coordinates1 mapValues {
    p => p - p0
  }
    
  val sizes: List[(Int, Int)] = go.layers.zipWithIndex.map {
    case (s, i) => i -> s.size
  }
  
  val baseGraph: Graph = go.category.baseGraph
  
  def draw[T](w: Int, h: Int, renderer: (String, Int, Int) => T): Iterable[T] = {
    
    val s = Pt(w / (x1-x0+2), h / (y1-y0+2))
    
    for {
      (obj, p) <- coordinates1
    } yield {
      val p1 = (p - middle).scale(s)
      val x = p1.x.toInt + w/2
      val y = (y1 - p1.y).toInt + h/2
      renderer(obj.toString, x, y)
    }
  }
}

case class Layout(category: Category) {
  val gradedObjects: Set[GradedObjects] = category.connectedComponents map GradedObjects
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
      goss = Layout(c).gradedObjects
      layouts = goss map Layout1
      l <- layouts
      gos = l.go
    } yield {
      val name = if (goss.size == 1) c.name else gos.category.name
      def asString[T](os: Set[T]): String = os.map(x => s""""$x"""").toList.sorted.mkString(",")
      val all = gos.layers map asString
      (name, l)
    }
    
    def svg(layout: Layout1, w: Int, h: Int): String = {
      
      val strings = layout.draw(w, h, (txt, x, y) =>
        s"""
           |<text x="${x-4}" y=${y+4}>$txt</text>
           |<circle cx="$x" cy="$y" r="10"
           | style="fill:yellow;stroke:black;stroke-width:2;opacity:0.3" />
           |""".stripMargin
      )
      strings mkString "\n"
    }

    def draw(layout: Layout1, w: Int, h: Int): String = {
      s"""<svg width="$w" height="$h">
         |  <rect x="0" y="0" rx="15" ry="15" width="$w" height="$h"
         |  style="fill:blue;stroke:black;stroke-width:5;opacity:0.03" />
         |
         |${svg(layout, w, h)}
         |</svg>
         |""".stripMargin
    }
    
    val htmlVersion = fullMap map {
      case (name, layout) =>
        val c = layout.coordinates
//        println(s)
//        println(name)
//        println(c)
        s"<h3>$name</h3>${draw(layout, 200, 200)}"
    } mkString("<hr/><p/>")

    println(html(htmlVersion))
    
    val out = new FileWriter("cats.html")
    out.write(html(htmlVersion))
    out.close
    val repr = fullMap.mkString("Map(\n  ", ",\n  ", "\n)")
  }
}