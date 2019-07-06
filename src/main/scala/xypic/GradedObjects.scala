package xypic

import math.cat.Category

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
