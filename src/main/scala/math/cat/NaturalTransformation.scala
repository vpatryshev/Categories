package math.cat

import scalakittens.Result
import scalakittens.Result.Outcome

/**
  * Natural transformation class: morphisms for functors.
  *
  * @tparam X functors domain category type
  * @tparam Y functors codomain category type
  * @param f        first functor
  * @param g        second functor
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
  ](val f: Functor[X, Y],
    val g: Functor[X, Y]) extends Morphism[Functor[X, Y], Functor[X, Y]] {
  
  def transformPerObject(x: f.d0.O): f.d1.Arrow

  override val d0: Functor[X, Y] = f
  override val d1: Functor[X, Y] = g
  def domainCategory: X = f.d0
  def codomainCategory: Y = f.d1
  
  def compose(
    next: NaturalTransformation[X, Y]
  ): NaturalTransformation[X, Y] = {

    val first: Functor[X, Y] = f
    val targetCategory = g.d1
    
    def comp(x: first.d0.O): targetCategory.Arrow = {
      val fHere: targetCategory.Arrow =
        transformPerObject(x.asInstanceOf[f.d0.O]).asInstanceOf[targetCategory.Arrow]
      val fThere: targetCategory.Arrow =
        next.transformPerObject(x.asInstanceOf[next.f.d0.O]).asInstanceOf[targetCategory.Arrow]
      val compOpt: Option[targetCategory.Arrow] = targetCategory.m(fHere, fThere)
      compOpt getOrElse(
          throw new IllegalArgumentException(s"Bad transformation for $x for $fHere and $fThere"))
    }
    
    new NaturalTransformation[X, Y](f, next.g) {
      def transformPerObject(x: f.d0.O): f.d1.Arrow =
        comp(x.asInstanceOf[first.d0.O]).asInstanceOf[f.d1.Arrow]
    }
  }
  
  private lazy val asMap: Map[f.d0.O, f.d1.Arrow] =
    (f.d0.objects map (o => o -> transformPerObject(o)) toMap) .asInstanceOf[Map[f.d0.O, f.d1.Arrow]]
  
  override lazy val hashCode: Int = f.hashCode | g.hashCode*17 | asMap.hashCode*31
  
  override def equals(x: Any): Boolean = x match {
    case other: NaturalTransformation[X, Y] =>
      f == other.f && g == other.g && asMap == other.asMap
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
  ): Outcome = Result.forValue {
    require(domainCategory == g.d0, s"Functors must be defined on the same categories")
    require(codomainCategory == g.d1, s"Functors must map to the same categories")
    for {
      a <- f.d0.arrows
    } {
      val x0: f.d0.O = f.d0.d0(a)
      val x1: f.d0.O = f.d0.d1(a)
      val fa: f.d1.Arrow = f.arrowsMapping(a)
      val ga: g.d1.Arrow = g.arrowsMapping(a.asInstanceOf[g.d0.Arrow]) // same thing
      val tx0: f.d1.Arrow = transformPerObject(x0)
      val tx1: f.d1.Arrow = transformPerObject(x1)
      val rightdown: Option[f.d1.Arrow] = f.d1.m(fa, tx1)
      val downright: Option[f.d1.Arrow] = f.d1.m(tx0, ga.asInstanceOf[f.d1.Arrow])
      require(rightdown == downright, s"Nat'l transform law broken for $a")
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
  ](from: Functor[X, Y],
    to: Functor[X, Y])
  (
    mappings: from.d0.O => from.d1.Arrow
  ): Result[NaturalTransformation[X, Y]] = {
    validate[X, Y](from, to, from.d0, from.d1)(mappings) returning 
    new NaturalTransformation[X, Y](from, to) {
      override def transformPerObject(x: f.d0.O): f.d1.Arrow =
        mappings(x.asInstanceOf[from.d0.O]).asInstanceOf[f.d1.Arrow]
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
      override def transformPerObject(x: f.d0.O): f.d1.Arrow =
        objectMap(x.asInstanceOf[functor.d0.O]).asInstanceOf[f.d1.Arrow]
    }
  }

}
