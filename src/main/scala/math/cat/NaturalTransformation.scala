package math.cat

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
  
  def transformPerObject(x: f.XObject): f.YArrow

  override val d0: Functor[X, Y] = f
  override val d1: Functor[X, Y] = g
  def domainCategory: X = f.d0
  def codomainCategory: Y = f.d1
  
  def validate(): Unit = {
    require(domainCategory == g.d0, s"Functors must be defined on the same categories")
    require(codomainCategory == g.d1, s"Functors must map to the same categories")
    for {
      a <- f.d0.arrows
    } {
      val x0: f.XObject = f.d0.d0(a)
      val x1: f.XObject = f.d0.d1(a)
      val fa: f.YArrow = f.arrowsMapping(a)
      val ga: g.YArrow = g.arrowsMapping(a.asInstanceOf[g.XArrow]) // same thing
      val tx0: f.YArrow = transformPerObject(x0)
      val tx1: f.YArrow = transformPerObject(x1)
      val rightdown: Option[f.YArrow] = f.d1.m(fa, tx1)
      val downright: Option[f.YArrow] = f.d1.m(tx0, ga.asInstanceOf[f.d1.Arrow])
      require(rightdown == downright, s"Nat'l transform law broken for $a")
    }
  }

  def compose(
    next: NaturalTransformation[X, Y]
  ): NaturalTransformation[X, Y] = {

    val first: Functor[X, Y] = f
    val targetCategory = g.codomain
    
    def comp(x: first.XObject): targetCategory.Arrow = {
      val fHere: targetCategory.Arrow =
        transformPerObject(x.asInstanceOf[f.XObject]).asInstanceOf[targetCategory.Arrow]
      val fThere: targetCategory.Arrow =
        next.transformPerObject(x.asInstanceOf[next.f.XObject]).asInstanceOf[targetCategory.Arrow]
      val compOpt: Option[targetCategory.Arrow] = targetCategory.m(fHere, fThere)
      compOpt getOrElse(
          throw new IllegalArgumentException(s"Bad transformation for $x for $fHere and $fThere"))
    }
    
    new NaturalTransformation[X, Y](first, next.g) {
      override def transformPerObject(x: first.XObject): first.YArrow = comp(x).asInstanceOf[first.YArrow]
    }
  }

  validate()
  
  private lazy val asMap: Map[f.XObject, f.YArrow] =
    (f.d0.objects map (o => o -> transformPerObject(o)) toMap) .asInstanceOf[Map[f.XObject, f.YArrow]]
  
  override lazy val hashCode: Int = f.hashCode | g.hashCode*17 | asMap.hashCode*31
  
  override def equals(x: Any): Boolean = x match {
    case other: NaturalTransformation[X, Y] =>
      f == other.f && g == other.g && asMap == other.asMap
    case otherwise => false
  }
}

object NaturalTransformation {

  /**
    * Builds a identity natural transformation id[f]: f -> f
    *
    * @param functor the functor for which we are building the identity transformation
    * @return identity natural transformation for the functor
    */
  def id[Dom <: Category[_,_], Codom <: Category[_,_]](functor: Functor[Dom, Codom]):
  NaturalTransformation[Dom, Codom] = {

    def objectMap(x: functor.XObject): functor.codomain.Arrow =
      functor.codomain.id(functor.objectsMapping(x).asInstanceOf[functor.codomain.Object])

    new NaturalTransformation[Dom, Codom](
      functor, functor) {
      override def transformPerObject(x: f.XObject): f.YArrow =
        objectMap(x.asInstanceOf[functor.XObject]).asInstanceOf[f.YArrow]
    }
  }

}
