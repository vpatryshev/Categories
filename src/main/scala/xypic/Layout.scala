package xypic

import math.cat.Category
import math.cat.Categories._

import scala.collection.mutable

case class GradedObjects(cat: Category) {
  val allArrows: Set[cat.Arrow] = cat.arrows
  
  def group(arrows: Set[cat.Arrow]): Map[Int, Set[cat.Obj]] = {
    val objWithSizes = arrows.groupBy(cat.d1).mapValues(_.size)
    val groupedBySize = objWithSizes.groupBy(_._2).mapValues(_.keySet)
    groupedBySize
  }
  
  def head(arrows: Set[cat.Arrow]): Set[cat.Obj] =
    group(arrows).toList.sortBy(_._1).headOption.map(_._2.toSet).toSet.flatten.toSet
  
  def arrowsNotTo(objs: Set[cat.Obj]): Set[cat.Arrow] = cat.arrows.filterNot(a => objs(cat.d1(a)))
  
  def arrowsFrom(o: cat.Obj): Set[cat.Arrow] =
    allArrows.filter(a => cat.d0(a) == o && cat.d1(a) != o)
  
  def next(objs: Set[cat.Obj] = Set.empty): (Set[cat.Obj], Set[cat.Obj]) = {
    val newOne = head(arrowsNotTo(objs))
    (objs union newOne, newOne)
  }
  
  lazy val layers: List[Set[cat.Obj]] = {
    Stream.iterate(next(Set.empty[cat.Obj])){
      case (sum, current) =>
        val (ns, nc) = next(sum)
        (ns, nc)
    } takeWhile(_._2.nonEmpty)
  } map (_._2) toList
}

case class Layout1(go: GradedObjects) {
  val name = go.cat.name
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
  
  val coordinates = coordinates0.flatten.toMap
    
  val sizes = go.layers.zipWithIndex.map {
    case (s, i) => i -> s.size
  }
  
  def print: Unit = {
    val x0 = coordinates.values.minBy(_._1)._1
    val x1 = coordinates.values.maxBy(_._1)._1
    val y0 = coordinates.values.minBy(_._2)._2
    val y1 = coordinates.values.maxBy(_._2)._2
    
    val buf = new StringBuilder(300)
    for (i <- 0 until 300) buf.append('.')
    
    for {
      (obj, (x, y)) <- coordinates
    } {
      val pos: Int = (x - x0) * 6 + (270 - (y - y0)*60)
      buf(pos) = obj.toString.head
    }
    println()
    println(buf.grouped(30).mkString("\n"))
  }
  
}

case class Layout(c: Category) {
  val gradedObjects: Set[GradedObjects] = c.connectedComponents map GradedObjects
}

object TestIt {
  
  def main(args: Array[String]): Unit = {
    val fullMap = for {
      c <- KnownFiniteCategories
      goss = Layout(c).gradedObjects
      gos <- goss
    } yield {
      val name = if (goss.size == 1) c.name else gos.cat.name
      val l = Layout1(gos)
      def asString[T](os: Set[T]): String = os.map(x => s""""$x"""").toList.sorted.mkString(",")
      val all = gos.layers map asString
      val s = s""""$name"->List(${all.map(x => s"Set($x)").mkString(", ")})"""
      println(s)
      println(l.coordinates)
      l.print
      s
    }
    
    val repr = fullMap.mkString("Map(\n  ", ",\n  ", "\n)")
  }
}