package xypic

import math.cat.{Category, Graph}
import math.cat.Categories._

import scala.collection.mutable

case class GradedObjects(graph: Graph) {
  val allArrows: Set[graph.Arrow] = graph.arrows
  
  def group(arrows: Set[graph.Arrow]): Map[Int, Set[graph.Node]] = {
    val objWithSizes = arrows.groupBy(graph.d1).mapValues(_.size)
    val groupedBySize = objWithSizes.groupBy(_._2).mapValues(_.keySet)
    groupedBySize
  }
  
  def head(arrows: Set[graph.Arrow]): Set[graph.Node] =
    group(arrows).toList.sortBy(_._1).headOption.map(_._2).toSet.flatten
  
  def arrowsNotTo(objs: Set[graph.Node]): Set[graph.Arrow] = graph.arrows.filterNot(a => objs(graph.d1(a)))

  def arrowsNotConnecting(objs: Set[graph.Node]): Set[graph.Arrow] = graph.arrows.filterNot(a => objs(graph.d1(a)) || objs(graph.d0(a)))
  
  def arrowsFrom(o: graph.Node): Set[graph.Arrow] =
    allArrows.filter(a => graph.d0(a) == o && graph.d1(a) != o)
  
  def next(objs: Set[graph.Node] = Set.empty): (Set[graph.Node], Set[graph.Node]) = {
    val newOne = head(arrowsNotConnecting(objs))
    (objs union newOne, newOne)
  }
  
  lazy val layers: List[Set[graph.Node]] = {
    Stream.iterate(next(Set.empty[graph.Node])){
      case (sum, current) =>
        val (ns, nc) = next(sum)
        (ns, nc)
    } takeWhile(_._2.nonEmpty)
  } map (_._2) toList
}

case class Layout1(go: GradedObjects) {
  val name: String = go.graph.name
  private val indexed = go.layers.zipWithIndex.map { case (s, i) => i -> s.toList.sortBy(_.toString)}.toMap
  private var dir = (1, 0)
  private var prevW = 1
  private val taken: mutable.Set[(Int, Int)] = new mutable.HashSet[(Int, Int)]()

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
      val newPoint = (i - j + scala.math.max(0, step._1 + (dw-1)/2), step._2 + (dw-1)/2 - j)
      val actual = if (taken(newPoint)) (newPoint._1, newPoint._2 - 1) else newPoint
      taken.add(actual)
      o -> actual
    }
    if (w > 2 * i + 1) dir = (1, 1)
    row
  }
  
  val coordinates: Map[go.graph.Node, (Int, Int)] = coordinates0.flatten.toMap
    
  val sizes: List[(Int, Int)] = go.layers.zipWithIndex.map {
    case (s, i) => i -> s.size
  }
  
  def print(): Unit = {
    val x0 = coordinates.values.minBy(_._1)._1
    val x1 = coordinates.values.maxBy(_._1)._1
    val y0 = coordinates.values.minBy(_._2)._2
    val y1 = coordinates.values.maxBy(_._2)._2
    
    val cx = 15 / (x1-x0+1)
    val cy = (5 / (x1-x0+1)) * 30
    
    val buf = new StringBuilder(300)
    for (i <- 0 until 300) buf.append('.')
    
    for {
      (obj, (x, y)) <- coordinates
    } {
      val pos: Int = (2*x - x0 - x1) * cx + 165 - (2*y - y0 - y1)*cy
      buf(pos) = obj.toString.head
    }
    println()
    println(buf.grouped(30).mkString("\n"))
  }
  
}

case class Layout(graph: Graph) {
  val gradedObjects: Set[GradedObjects] = graph.connectedComponents map GradedObjects
}

object TestIt {
  
  def main(args: Array[String]): Unit = {
    val fullMap = for {
      c <- KnownFiniteCategories
      goss = Layout(c).gradedObjects
      gos <- goss
    } yield {
      val name = if (goss.size == 1) c.name else gos.graph.name
      val l = Layout1(gos)
      def asString[T](os: Set[T]): String = os.map(x => s""""$x"""").toList.sorted.mkString(",")
      val all = gos.layers map asString
      val s = s""""$name"->List(${all.map(x => s"Set($x)").mkString(", ")})"""
      println(s)
      println(l.coordinates)
      l.print()
      s
    }
    
    val repr = fullMap.mkString("Map(\n  ", ",\n  ", "\n)")
  }
}