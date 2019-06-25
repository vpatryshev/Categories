package xypic

import math.cat.Category
import math.cat.Categories._

case class Layout1(cat: Category) {
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
  
  def graded: List[Set[cat.Obj]] = {
    Stream.iterate(next(Set.empty[cat.Obj])){
      case (sum, current) =>
        val (ns, nc) = next(sum)
        (ns, nc)
    } takeWhile(_._2.nonEmpty)
  } map (_._2) toList
}

case class Layout(c: Category) {
  val layouts: Set[Layout1] = c.connectedComponents map Layout1
}

object TestIt {
  def main(args: Array[String]): Unit = {
    val fullMap = for {
      c <- KnownFiniteCategories
      ls = Layout(c).layouts
      l <- ls
    } yield {
      val name = if (ls.size == 1) c.name else l.cat.name
      def asString[T](os: Set[T]): String = os.map(x => s""""$x"""").toList.sorted.mkString(",")
      val all = l.graded map asString
      s""""$name"->List(${all.map(x => s"Set($x)").mkString(", ")})"""
    }
    
    val repr = fullMap.mkString("Map(\n  ", ",\n  ", "\n)")
    
    println(repr)
  }
}