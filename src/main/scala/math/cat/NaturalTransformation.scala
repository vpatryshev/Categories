package math.cat

import scalakittens.Result
import Result._
import scalakittens.Result.Outcome

/**
  * Natural transformation class: morphisms for functors.
  *
  * The following three requirements are checked:
  * f and g are from the same category
  * f and g are to the same category
  * the following squares are commutative:
  *    f[a]: f[x] ---> f[y]
  *            |         |
  *       t[x] |         | t[y]
  *            |         |
  *            V         V
  *    g[a]: g[x] ---> g[y]
  */
abstract class NaturalTransformation extends Morphism[Functor, Functor] { self ⇒

  lazy val domainCategory: Category = d0.d0
  lazy val codomainCategory: Category = d1.d1 // == d0.d1, of course

  def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow

  // TODO: check the preconditions, return an option
  def compose(next: NaturalTransformation): NaturalTransformation = {
    
    def comp(x: domainCategory.Obj): codomainCategory.Arrow = {
      val fHere: codomainCategory.Arrow =
        codomainCategory.arrow(transformPerObject(domainCategory.obj(x)))
      val fThere: codomainCategory.Arrow =
        codomainCategory.arrow(next.transformPerObject(next.domainCategory.obj(x)))
      val compOpt: Option[codomainCategory.Arrow] = codomainCategory.m(fHere, fThere)
      compOpt getOrElse(
          throw new IllegalArgumentException(s"Bad transformation for $x for $fHere and $fThere"))
    }

    def composed[T](x: T) = {
      comp(domainCategory.obj(x))
    }

    new NaturalTransformation {
      val d0: Functor = self.d0
      val d1: Functor = next.d1
      def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow = codomainCategory.arrow(composed(x))
    }
  }
  
  private lazy val asMap: Map[domainCategory.Obj, codomainCategory.Arrow] =
    domainCategory.objects map (o ⇒ o -> transformPerObject(o)) toMap
  
  override lazy val hashCode: Int = d0.hashCode | d1.hashCode*17 | asMap.hashCode*31
  
  override def equals(x: Any): Boolean = x match {
    case other: NaturalTransformation ⇒
      d0 == other.d0 && d1 == other.d1 && asMap == other.asMap
    case otherwise ⇒ false
  }
}

object NaturalTransformation {

  def validate[
  X <: Category,
  Y <: Category
  ](
    f: Functor, g: Functor, domainCategory: Category, codomainCategory: Category)(
    transformPerObject: f.d0.Obj ⇒ f.d1.Arrow
  ): Outcome =
    OKif(domainCategory == g.d0, s"Functors must be defined on the same categories") andAlso
    OKif(codomainCategory == g.d1, s"Functors must map to the same categories") andAlso
    Result.traverse {
    for {
      a <- f.d0.arrows
    } yield {
      val x0: f.d0.Obj = f.d0.d0(a)
      val x1: f.d0.Obj = f.d0.d1(a)
      val rr = for {
        fa <- forValue(f.arrowsMapping(a))
        ga <- forValue(g.arrowsMapping(g.d0.arrow(a)))
      } yield Result.forValue {
        val tx0: f.d1.Arrow = transformPerObject(x0)
        val tx1: f.d1.Arrow = transformPerObject(x1)
        val rightdown: Option[f.d1.Arrow] = f.d1.m(fa, tx1)
        val downright: Option[f.d1.Arrow] = f.d1.m(tx0, f.d1.arrow(ga))
        require(rightdown == downright, s"Nat'l transform law broken for $a")
      }
      
      rr.flatten
    }
  }



  /**
    * Builds a natural transformation
    *
    * @param from0 first functor
    * @param to0   second functor
    * @param mappings a set morphism that for each domain object x returns f(x) -> g(x)
    */
  def build(from0: Functor, to0: Functor)
  (
    mappings: from0.d0.Obj ⇒ from0.d1.Arrow
  ): Result[NaturalTransformation] = {
    validate(from0, to0, from0.d0, from0.d1)(mappings) returning 
    new NaturalTransformation {
      val d0: Functor = from0
      val d1: Functor = to0
      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
        codomainCategory.arrow(mappings(from0.d0.obj(x)))
    }
  }

  /**
    * Builds an identity natural transformation id[f]: f -> f
    *
    * @param functor the functor for which we are building the identity transformation
    * @return identity natural transformation for the functor
    */
  def id(functor: Functor): NaturalTransformation = {

    def objectMap(x: functor.d0.Obj): functor.d1.Arrow =
      functor.d1.id(functor.d1.obj(functor.objectsMapping(x)))

    new NaturalTransformation {
      val d0: Functor = functor
      val d1: Functor = functor
      
      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
        codomainCategory.arrow(objectMap(functor.d0.obj(x)))
    }
  }

}
