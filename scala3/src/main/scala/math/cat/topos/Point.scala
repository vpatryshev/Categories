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

  private val domainCategory: Category = topos.domain

  def apply(x: Any): Any = mapping(x)
  
  def named(name: Any): Point = new Point(name, topos, mapping)

  def asDiagram: Diagram = {
    new Diagram(tag, topos) { diagram =>

      override val objectsMapping: d0.Obj => d1.Obj =
        (x: d0.Obj) => d1.obj(Set(mapping(domainCategory.obj(x))))

      private def arrowToFunction(a: d0.Arrow): Any => Any =
        (z: Any) => {
          val y = domainCategory.obj(d0.d1(a))
          val v = mapping(y)
          v
        }

      override protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow = (a: d0.Arrow) => {
        d1.arrow( // need a set function from a.d0 to a.d1
          SetFunction(s"${diagram.tag}(.)", objectsMapping(d0.d0(a)), objectsMapping(d0.d1(a)), arrowToFunction(a))
        )
      }
    }
  }
}


