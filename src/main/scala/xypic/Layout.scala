package xypic

import math.cat.Category
import math.cat.Categories._

case class Layout(cat: Category) {
  val allArrows: Set[cat.Arrow] = cat.arrows
  
  def sort(arrows: Set[cat.Arrow]): List[(cat.Obj, Int)] =
    arrows.groupBy(cat.d1).mapValues(_.size).toList.sortBy(_._1.toString).sortBy(_._2)
  
  def arrowsFrom(o: cat.Obj): Set[cat.Arrow] =
    allArrows.filter(a => cat.d0(a) == o && cat.d1(a) != o)
  
  val sorted: List[(cat.Obj, Int)] = sort(allArrows)
  val first: Option[cat.Obj] = sorted.headOption.map(_._1)
  
  val next = first flatMap (o => sort(arrowsFrom(o)).headOption)
}

object TestIt {
  def main(args: Array[String]): Unit = {
    for {
      c <- KnownFiniteCategories
    } {
      val l = Layout(c)
      println(s"${c.name} -> ${l.first}; ${l.next}")
    }
  }
}