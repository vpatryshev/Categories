package math.cat

import math.Base._
import math.sets.Functions._
import math.sets.Sets
import math.sets.Sets._
import scalakittens.{Bad, Result}
import scalakittens.Result._

/**
  * Functor class: functions for categories.
  *
  * @param d0 domain
  * @param d1 codomain
  */
abstract class Functor(
  val tag: Any,
  val d0: Category, val d1: Category
) extends GraphMorphism {

  def domainObjects: d0.Objects = d0.objects

  val objectsMapping: d0.Obj => d1.Obj
  protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow
  
  override def toString: String = s"Functor $tag"

  override def arrowsMapping(a: d0.Arrow): d1.Arrow = {
    val domainX: d0.Obj = d0.d0(a)
    try {
      if (d0.isIdentity(a)) {
        d1.id(objectsMapping(domainX))
      } else arrowsMappingCandidate(a)
    } catch {
      case x: Exception =>
        throw new IllegalArgumentException(
          s"$tag: arrow mapping not found for $a: $domainX -> ${d0.d1(a)}", x)
    }
  }

  override def nodesMapping(n: d0.Node): d1.Node =
    d1.obj(objectsMapping(d0.obj(n)))

}

object Functor {

  /**
    * Factory method. Builds identity functor for a category (identity functor).
    *
    * @param c the category
    * @return identity functor on the given category
    */
  def id(c: Category): Functor = new Functor("id", c, c) {
      override val objectsMapping: d0.Obj => d1.Obj = (x: d0.Obj) => d1.obj(x)

      override protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow =
        (a: d0.Arrow) => d1.arrow(a)

      override def nodesMapping(n: d0.Node): d1.Node = d1.node(n)
    }

  /**
    * Builds a constant functor from a category to an object in another.
    *
    * @param x  the category
    * @param y  second category
    * @param y0 an object in category y
    * @return constant functor on x that takes maps all objects to y0 and all arrows to y0's identities.
    */
  def const(x: Category, y: Category)(y0: y.Obj): Functor =
    unsafeBuild( // won't fail? Check y0, at least
      y.toString, x, y)(
      SetMorphism.const(x.objects, y.objects, y0),
      SetMorphism.const(x.arrows, y.arrows, y.id(y0)))

  private def unsafeBuild(
    tag: String,
    dom: Category,
    codom: Category)(
    objectsMorphism: dom.Obj => codom.Obj,
    arrowsMorphism: dom.Arrow => codom.Arrow): Functor =
    new Functor(tag, dom, codom) {
      override val objectsMapping: d0.Obj => d1.Obj = (x: d0.Obj) => d1.obj(objectsMorphism(dom.obj(x)))

      override protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow =
        (a: d0.Arrow) => d1.arrow(arrowsMorphism(dom.arrow(a)))
    }

  /**
    * Builds a functor, given the data:
    *
    * @param atag           functor tag
    * @param dom            domain category
    * @param codom          codomain category
    * @param objectsMapping objects mapping
    * @param arrowsMapping  arrows mapping
    * @return
    */
  def apply(
    atag: String,
    dom: Category,
    codom: Category)(
    objectsMapping: dom.Obj => codom.Obj,
    arrowsMapping: dom.Arrow => codom.Arrow): Result[Functor] = {
    validateFunctor(unsafeBuild(atag, dom, codom)(objectsMapping, arrowsMapping))
  }

  /**
    * Validates a functor candidate.
    * A functor is valid if it is valid as a graph function, and besides,
    * it preserves identities and arrows composition.
    * That is, F(id(x)) == id(F(x)), and
    * F(g) ∘ F(f) = F(g ∘ f)
    */
  private[cat] def validateFunctor(f: Functor): Result[Functor] = for {
    _ <- checkObjectMapping(f)
    _ <- checkArrowMapping(f)
    _ <- checkIdentityPreservation(f) andAlso checkCompositionPreservation(f)
  } yield f

  private def checkIdentityPreservation(f: Functor): Outcome = Result.traverse {
    for (x <- f.domainObjects) yield {
      val y: f.d1.Obj = f.objectsMapping(x)
      OKif(f.arrowsMapping(f.d0.id(x)) == f.d1.id(y), s"Identity must be preserved for $x ↦ $y")
    }

  } andThen OK

  private def checkCompositionPreservation(f: Functor): Outcome = {
    def check(fx: f.d0.Arrow, gx: f.d0.Arrow): Outcome = {
      val fy = f.arrowsMapping(fx)
      val gy = f.arrowsMapping(gx)
      val gy_fy = f.d1.m(fy, gy)
      val gx_fx = f.d0.m(fx, gx)
      val expected = gx_fx map (gf => f.arrowsMapping(gf))
      OKif(gy_fy == expected,
        s"Functor must preserve composition (failed on $fx, $fy, $gx, $gy, $gy_fy; $expected)")
    }

    val checked = Result.traverse {
      for {
        (fx, gx) <- Category.composablePairs(f.d0)
      } yield check(fx, gx)
    }
    
    checked andThen OK
  }

  private def checkArrowMapping(f: Functor): Outcome = Result.traverse {
    for (a <- f.d0.arrows) yield {
      Result.forValue(f.arrowsMapping(a)) flatMap {
        aa =>
          val domainActual = f.d1.d0(aa)
          val codomainActual = f.d1.d1(aa)
          val domainExpected = f.objectsMapping(f.d0.d0(a))
          val codomainExpected = f.objectsMapping(f.d0.d1(a))
          OKif(domainActual == domainExpected,
            s"Inconsistent mapping for d0($a) - $domainActual vs $domainExpected") andAlso
            OKif(codomainActual == codomainExpected,
              s"Inconsistent mapping for d1($a) - $codomainActual vs $codomainExpected")
      }
    }

  } andThen OK

  private def checkObjectMapping(f: Functor): Outcome =
    Result.traverse {
      for (x <- f.domainObjects) yield {
        val someY: Result[f.d1.Obj] =
          Result.forValue(f.objectsMapping(x)) orCommentTheError s"Object mapping fails for $x"
        someY.filter(f.d1.objects, s"Object mapping defined incorrectly for $x")
      }
    } andThen OK
}