package xypic

import math.cat.{Category, Graph}
import math.cat.Categories._

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
  
  val coordinates: Map[go.category.Node, (Int, Int)] = coordinates0.flatten.toMap
    
  val sizes: List[(Int, Int)] = go.layers.zipWithIndex.map {
    case (s, i) => i -> s.size
  }
  
  val baseGraph = go.category.baseGraph
  
  def print(): Unit = {
    val xys0 = coordinates.values.toList
    val xys = xys0.headOption.getOrElse((0, 0)) :: xys0
    val x0 = xys.minBy(_._1)._1
    val x1 = xys.maxBy(_._1)._1
    val y0 = xys.minBy(_._2)._2
    val y1 = xys.maxBy(_._2)._2
    
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

case class Layout(category: Category) {
  val gradedObjects: Set[GradedObjects] = category.connectedComponents map GradedObjects
}

object TestIt {
  
  def main(args: Array[String]): Unit = {
    val fullMap = for {
      c <- KnownFiniteCategories
      goss = Layout(c).gradedObjects
      gos <- goss
    } yield {
      val name = if (goss.size == 1) c.name else gos.category.name
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