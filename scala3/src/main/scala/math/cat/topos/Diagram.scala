package math.cat.topos

import scala.language.implicitConversions
import math.Base._
import math.cat._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.sets.Functions._
import math.sets.Sets.{set, _}
import math.sets.{FactorSet, Sets}
import scalakittens.{Good, Result}

import scala.collection.MapView
import scala.language.postfixOps

/**
  * Diagram from a category to Categories.SETF.
  *
  * The term "point" below means a point in categorical meaning:
  * an arrow from a terminal object into a given object.
  * Here we talk about the category of diagrams, so a point is a
  * singleton diagram. It must have been a mapping from objects of the base category to values
  * that are elements of the sets: given a diagram D, p(x) \in D(x).
  */
abstract class Diagram(
  tag: Any,
  val topos: GrothendieckTopos)
  extends Functor(tag, topos.domain, SetCategory.Setf) { diagram =>
//  val d0: Category = d0
  type XObject = d0.Obj
  type XObjects = Set[XObject]
  type XArrow = d0.Arrow
  type XArrows = Set[XArrow]
  
  implicit def setOf(x: d1.Obj): set = x.asInstanceOf[set]

  private[topos] def setAt(x: Any): set = setOf(objectsMapping(d0.asObj(x)))
  def apply(x: Any): set = setOf(objectsMapping(d0.obj(x)))

}

object Diagram {

  private[topos] def apply[O, A](tag: Any, t: GrothendieckTopos)(
    objectsMap: O => set,
    arrowMap:   t.domain.Arrow => SetFunction): Diagram = {

    new Diagram(tag.toString, t) {
      
      override private[topos] def setAt(x: Any): set =
        d1.asObj(objectsMap(x.asInstanceOf[O])) // TODO: get rid of Any and casting
      
      override val objectsMapping: XObject => d1.Obj = (o: XObject) => {
        val x = o.asInstanceOf[O] // TODO: get rid of casting
        val y = objectsMap(x)
        d1.asObj(y) // TODO: get rid of casting
      }

      override protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow =
        (a: d0.Arrow) => d1.arrow(arrowMap(a))
    }
  }
}
