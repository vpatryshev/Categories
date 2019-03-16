package math.cat

import scalakittens.Result
import Result._
import scalakittens.Result.Outcome

/**
  * Natural transformation class: morphisms for functors.
  *
  * @tparam X functors domain category type
  * @tparam Y functors codomain category type
  * @param from        first functor
  * @param to        second functor
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
abstract class NaturalTransformation[
    X <: Category[_, _],
    Y <: Category[_, _]
  ](val from: Functor[X, Y],
    val to: Functor[X, Y]) extends Morphism[Functor[X, Y], Functor[X, Y]] {
  
  def transformPerObject(x: from.d0.O): from.d1.Arrow

  override val d0: Functor[X, Y] = from
  override val d1: Functor[X, Y] = to
  def domainCategory: X = from.d0
  def codomainCategory: Y = from.d1
  
  def compose(
    next: NaturalTransformation[X, Y]
  ): NaturalTransformation[X, Y] = {

    val first: Functor[X, Y] = from
    val targetCategory = to.d1
    
    def comp(x: first.d0.O): targetCategory.Arrow = {
      val fHere: targetCategory.Arrow =
        transformPerObject(x.asInstanceOf[from.d0.O]).asInstanceOf[targetCategory.Arrow]
      val fThere: targetCategory.Arrow =
        next.transformPerObject(x.asInstanceOf[next.from.d0.O]).asInstanceOf[targetCategory.Arrow]
      val compOpt: Option[targetCategory.Arrow] = targetCategory.m(fHere, fThere)
      compOpt getOrElse(
          throw new IllegalArgumentException(s"Bad transformation for $x for $fHere and $fThere"))
    }
    
    new NaturalTransformation[X, Y](from, next.to) {
      def transformPerObject(x: from.d0.O): from.d1.Arrow =
        comp(x.asInstanceOf[first.d0.O]).asInstanceOf[from.d1.Arrow]
    }
  }
  
  private lazy val asMap: Map[from.d0.O, from.d1.Arrow] =
    (from.d0.objects map (o => o -> transformPerObject(o)) toMap) .asInstanceOf[Map[from.d0.O, from.d1.Arrow]]
  
  override lazy val hashCode: Int = from.hashCode | to.hashCode*17 | asMap.hashCode*31
  
  override def equals(x: Any): Boolean = x match {
    case other: NaturalTransformation[X, Y] =>
      from == other.from && to == other.to && asMap == other.asMap
    case otherwise => false
  }
}

object NaturalTransformation {

  def validate[
  X <: Category[_, _],
  Y <: Category[_, _]
  ](
    f: Functor[X, Y], g: Functor[X, Y], domainCategory: X, codomainCategory: Y)(
    transformPerObject: f.d0.O => f.d1.Arrow
  ): Outcome =
    OKif(domainCategory == g.d0, s"Functors must be defined on the same categories") andAlso
    OKif(codomainCategory == g.d1, s"Functors must map to the same categories") andAlso
    Result.traverse {
    for {
      a <- f.d0.arrows
    } yield {
      val x0: f.d0.O = f.d0.d0(a)
      val x1: f.d0.O = f.d0.d1(a)
      val rr = for {
        fa <- forValue(f.arrowsMapping(a))
        ga <- forValue(g.arrowsMapping(a.asInstanceOf[g.d0.Arrow])) // same thing
      } yield Result.forValue {
        val tx0: f.d1.Arrow = transformPerObject(x0)
        val tx1: f.d1.Arrow = transformPerObject(x1)
        val rightdown: Option[f.d1.Arrow] = f.d1.m(fa, tx1)
        val downright: Option[f.d1.Arrow] = f.d1.m(tx0, ga.asInstanceOf[f.d1.Arrow])
        require(rightdown == downright, s"Nat'l transform law broken for $a")
      }
      
      rr.flatten
    }
  }



  /**
   * @tparam X functors domain category type
   * @tparam Y functors codomain category type
   * @param from first functor
   * @param to   second functor
   * @param mappings a set morphism that for each domain object x returns f(x) -> g(x)
   */
  def build[
    X <: Category[_, _],
    Y <: Category[_, _]
  ](from0: Functor[X, Y],
    to0: Functor[X, Y])
  (
    mappings: from0.d0.O => from0.d1.Arrow
  ): Result[NaturalTransformation[X, Y]] = {
    validate[X, Y](from0, to0, from0.d0, from0.d1)(mappings) returning 
    new NaturalTransformation[X, Y](from0, to0) {
      override def transformPerObject(x: from.d0.O): from.d1.Arrow =
        mappings(x.asInstanceOf[from0.d0.O]).asInstanceOf[from.d1.Arrow]
    }
  }

  /**
    * Builds an identity natural transformation id[f]: f -> f
    *
    * @param functor the functor for which we are building the identity transformation
    * @return identity natural transformation for the functor
    */
  def id[X <: Category[_, _], Y <: Category[_, _]](functor: Functor[X, Y]):
  NaturalTransformation[X, Y] = {

    def objectMap(x: functor.d0.O): functor.d1.Arrow =
      functor.d1.id(functor.objectsMapping(x).asInstanceOf[functor.d1.O])

    new NaturalTransformation[X, Y](
      functor, functor) {
      override def transformPerObject(x: from.d0.O): from.d1.Arrow =
        objectMap(x.asInstanceOf[functor.d0.O]).asInstanceOf[from.d1.Arrow]
    }
  }

}
