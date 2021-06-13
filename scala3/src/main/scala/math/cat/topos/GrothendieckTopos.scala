package math.cat.topos

import scala.language.postfixOps
import reflect.Selectable.reflectiveSelectable
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.{Category, CategoryData, Functor, NaturalTransformation, SetFunction}
import math.sets.Functions
import math.sets.Sets
import math.sets.Sets._
import scalakittens.Result
import Result._
import math.Base.concat
import math.cat.SetMorphism

import scala.collection.mutable

// see also http://www.cs.man.ac.uk/~david/categories/book/book.pdf - ML implementation of topos

trait GrothendieckTopos
{ topos =>
  type Obj = Diagram
  type Arrow = DiagramArrow
   
  val domain: Category

  type Mapping = domain.Obj => Any => Any

  /**
    * Subobject classifier. Ω is "Option-Z" on your Mac.
    */
  object Ω extends Diagram("Ω", this) { Ω =>
    // For each object `x` we produce a set of all subobjects of `Representable(x)`.
    // These are values `Ω(x)`. We cache them in the following map map `x => Ω(x)` .
    private[topos] val subrepresentablesIndexed: Map[domain.Obj, Set[Diagram]] = null //subobjectsOfRepresentables

    // this one is consumed by Functor constructor
    val objectsMapping: d0.Obj => d1.Obj =
      (d: d0.Obj) => d1.obj(subrepresentablesIndexed(domain.obj(d): domain.Obj))

    // for each arrow `a: x -> y` produce a transition `Ω(x) -> Ω(y)`.
    private def am(a: domain.Arrow): SetFunction = {
      val x = domain.d0(a)
      val y = domain.d1(a)
      val d0 = subrepresentablesIndexed(x) // `Ω(x)` = all subobjects of `Representable(x)`
      val d1 = subrepresentablesIndexed(y) // `Ω(y)` = all subobjects of `Representable(y)`

      // How one diagram is transformed via `a`:
      // For each `rx ⊂ Repr(x)` we have to produce a diagram `ry ⊂ Repr(y)`
      def diaMap(rx: Diagram): Diagram /*a subrepresentable on `x`*/ = {
        // this is how elements of objects projections, that is, subterminals, are transformed by `a`
        def om1(o: domain.Obj): set = transformingOfSubrepresentables(a, rx)(o)
        def om2(o: Ω.topos.domain.Obj): set = om1(domain.asObj(o))

        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am1(b: Ω.topos.domain.Arrow): SetFunction = {
          val same_b = domain.arrow(b)
          val x1 = om1(domain.d0(same_b)) //  {f ∈ hom(y, d0(b)) | a compose f ∈ r1(d0(b)}
          val y1 = om1(domain.d1(same_b)) //  {f ∈ hom(y, d1(b)) | a compose f ∈ r1(d1(b)}

          // A function fom x1 to y1 - it does the transition
          new SetFunction("", x1, y1, g => domain.m(domain.arrow(g), same_b).get)
        }

        Diagram("", topos)(om2, am1) // no validation, we know it's ok
      }

      // no validation here, the function is known to be ok
      new SetFunction(s"[$a]", d0.untyped, d1.untyped, d => diaMap(d.asInstanceOf[Diagram]))
    }

    protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow =
      (a: XArrow) => d1.arrow(am(domain.arrow(a)))

    /**
      * Given an arrow `a`, 
      * {f ∈ hom(y, x1) | a compose f ∈ r1(x1)}
      *
      * @param a  an arrow
      * @param rx a subrepresentable
      * @param x1 an object in domain (a "state")
      * @return
      */
    private def transformingOfSubrepresentables(a: domain.Arrow, rx: Diagram)(x1: domain.Obj): set = {
      val y = domain.d1(a)
      val rx_at_x1 = rx(x1)
      for {
        f <- domain.hom(y, x1)
        candidate <- domain.m(a, domain.arrow(f))
        if rx_at_x1 contains candidate
      } yield f
    }
  }
}
