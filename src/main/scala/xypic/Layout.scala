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
  
  val (f, first) = next()
  val (s, second) = next(f)
  val (t, third) = next(s)
  
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
    for {
      c <- KnownFiniteCategories
      ls = Layout(c).layouts
      l <- ls
    } {
      val name = if (ls.size == 1) c.name else l.cat.name
      def asString[T](os: Set[T]): String = os.map(_.toString).toList.sorted.mkString(",")
      val first = asString(l.first)
      val next = asString(l.second)
      val third = asString(l.third)
      println(s"$name -> $first; $next; $third")
      val all = l.graded map asString
      
      println(s"  ${all.mkString("; ")}")
    }
  }
}