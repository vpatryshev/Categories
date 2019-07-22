package xypic

import math.cat.Category

case class GradedObjects(category: Category) {
  private val allArrows: category.Arrows = category.arrows
  
  private def groupByNumberOfIncoming(arrows: category.Arrows): Map[Int, category.Objects] = {
    val objWithSizes: Map[category.Obj, Int] = arrows.groupBy(category.d1).mapValues(_.size)
    objWithSizes.groupBy(_._2).mapValues(_.keySet)
  }
  
  private def objectsWithSmallersNumberOfIncoming(arrows: category.Arrows): category.Objects =
    groupByNumberOfIncoming(arrows).toList.sortBy(_._1).headOption.map(_._2).toSet.flatten

  private def arrowsNotConnecting(objs: category.Objects): category.Arrows =
    category.arrows.filterNot(a => objs(category.d1(a)) || objs(category.d0(a)))
  
  private def nextLayer(objs: category.Objects): (category.Objects, category.Objects) = {
    val newOne = objectsWithSmallersNumberOfIncoming(arrowsNotConnecting(objs))
    (objs union newOne, newOne)
  }
  
  lazy val layers: List[category.Objects] = {
    Stream.iterate(nextLayer(Set.empty[category.Obj])){
      case (sum, current) =>
        val (ns, nc) = nextLayer(sum)
        (ns, nc)
    } takeWhile(_._2.nonEmpty)
  } map (_._2) toList
  
  lazy val layersOfClusters: List[Set[category.Objects]] = {
    layers map(layer => layer.map(obj => category.clusters(obj)))
  }
}
