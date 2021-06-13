package math.cat.topos

import math.cat.{Category, SetFunction}
import math.cat.topos.CategoryOfDiagrams.DiagramArrow

/**
  * A point of a topos object (of a diagram in a Grothendieck topos)
  * @param tag used to visually identify a point
  * @param topos the topos
  * @param mapping for each domain object, choose something in the topos diagram
  */
class Point(
  val tag: Any,
  val topos: GrothendieckTopos,
  val mapping: Any => Any) extends (Any => Any) { p =>

  def apply(x: Any): Any = mapping(x)

}


