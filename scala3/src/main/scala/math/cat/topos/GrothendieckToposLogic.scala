package math.cat.topos

import math.cat.SetFunction
import math.cat.topos.CategoryOfDiagrams.{BaseCategory, DiagramArrow, const}
import math.sets.Sets.{set, setOf}
import scalakittens.Result

import scala.collection.mutable
import scala.language.postfixOps
import math.cat.Morphism

trait GrothendieckToposLogic {
  topos: GrothendieckTopos =>

// TODO: use the right tagging, find the right predicate
//  private[this] val cache = mutable.Map[(String, Predicate, Predicate), Predicate]()

  trait Predicate extends DiagramArrow { p: DiagramArrow =>
    val d1: Diagram = Ω

    private def wrapTag(tag: Any): String = {
      val ts = tag.toString
      if (ts.contains("∧") || ts.contains("∨") || ts.contains("=>")) s"($ts)" else ts
    }

    private def tag2(tag1: Any, op: String, tag2: Any): String = s"${wrapTag(tag1)} $op ${wrapTag(tag2)}"

    private def setAt(o: Any): set = {
      val o1 = domainCategory.obj(o)
      val function = p.transformPerObject(o1).asInstanceOf[SetFunction]
      setOf(function.d0)
    }

    private def transformAt(o: Any): SetFunction =
      transformPerObject(domainCategory.obj(o)).asInstanceOf[SetFunction]
  }

}
