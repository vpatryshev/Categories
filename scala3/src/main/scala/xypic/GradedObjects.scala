package xypic

import math.cat.Category
import math.geometry2d.{GroupOfObjects, Pt, Rational}

import scala.language.postfixOps

case class GradedObjects(category: Category) {
  private val allArrows: category.Arrows = category.arrows
  
  private def groupByNumberOfIncoming(arrows: category.Arrows): Map[Int, category.Objects] = {
    val objWithSizes: Map[category.Obj, Int] = arrows.groupBy(category.d1).view.mapValues(_.size).toMap
    objWithSizes.groupBy(_._2).map {
      case (k, v) => k -> v.map(_._1).toSet
    } toMap
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
    LazyList.iterate(nextLayer(Set.empty[category.Obj])){
      case (sum, current) =>
        val (ns, nc) = nextLayer(sum)
        (ns, nc)
    } takeWhile(!_._2.isEmpty)
  } map (_._2) toList
  
  def clusterDiameter(size: Int): Int = size match
    case 1 => 1
    case 2 => 1
    case n =>
      val da = 2 * Math.PI / size
      val step = 3
      val d = step / Math.sin(da/2)
      (d + 0.5).toInt
  
  case class Cluster(objects: Set[category.Obj]) {
    def listObjects: List[category.Obj] = objects.toList
    def nameObjects = objects map (_.toString)
    
    def allocateAt(coords: Pt): Set[(String, Pt)] = objects.size match
      case 0 | 1 => objects.map(obj => obj.toString -> coords).toSet
      case n =>
        GroupOfObjects(objects.map(_.toString)).
          arrangeInCircle(coords, Rational(diameter, 2))

    val size: Int = objects.size
    lazy val code: String = s"$size.$objects"
    lazy val diameter: Int = clusterDiameter(size)
  }
  
  lazy val layersOfClusters: List[List[Cluster]] = {
    layers map(layer => layer.map(obj => Cluster(category.clusters(obj))).toList.sortBy(_.code))
  }
  
  lazy val nameObjectsInLayers: List[List[Set[String]]] = 
    layersOfClusters map { _ map  (_.nameObjects) }
}
