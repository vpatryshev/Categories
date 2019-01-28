package math.cat

/**
  * Natural transformation class: morphisms for functors.
  *
  * @tparam XObjects functors domain object type
  * @tparam XArrows functors domain arrow type
  * @tparam YObjects functors codomain object type
  * @tparam YArrows functors codomain arrow type
  * @param f        first functor
  * @param g        second functor
  * @param transformPerObject a set morphism that for each domain object x returns f(x) -> g(x)
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
case class NaturalTransformation[
    X <: Category[_, _],
    Y <: Category[_, _],
  ](
     f: Functor[X, Y],
     g: Functor[X, Y])(
     transformPerObject: f.XObject => f.YArrows
 ) extends Morphism[
  Functor[X, Y],
  Functor[X, Y]] {
  override val d0: Functor[XObjects, XArrows, YObjects, YArrows] = f
  override val d1: Functor[XObjects, XArrows, YObjects, YArrows] = g
  def domainCategory: Category[XObjects, XArrows] = f.domain
  def codomainCategory: Category[YObjects, YArrows] = f.codomain
  
  def validate(): Unit = {
    require(domainCategory == g.d0, s"Functors must be defined on the same categories")
    require(codomainCategory == g.d1, s"Functors must map to the same categories")
    for {
      a <- f.d0.arrows
    } {
      val x0: XObjects = f.d0.d0(a)
      val x1: XObjects = f.d0.d1(a)
      val fa: YArrows = f.arrowsMapping(a)
      val ga: YArrows = g.arrowsMapping(a)
      val tx0: YArrows = transformPerObject(x0)
      val tx1: YArrows = transformPerObject(x1)
      val rightdown: Option[YArrows] = codomainCategory.m(fa, tx1)
      val downright: Option[YArrows] = codomainCategory.m(tx0, ga)
      require(rightdown == downright, s"Nat'l transform law broken for $a")
    }
  }

  def compose(
    next: NaturalTransformation[XObjects, XArrows, YObjects, YArrows]
  ): NaturalTransformation[XObjects, XArrows, YObjects, YArrows] = {
    val targetCategory = g.codomain
    def comp(x: XObjects) = {
      val fHere = transformPerObject(x)
      val fThere = next.transformPerObject(x)
      val compOpt = targetCategory.m(fHere, fThere)
      compOpt getOrElse(
          throw new IllegalArgumentException(s"Bad transformation for $x for $fHere and $fThere"))
    }
    
    new NaturalTransformation[XObjects, XArrows, YObjects, YArrows](f, next.g, comp)
  }

  validate()
  
  private lazy val asMap: Map[XObjects, YArrows] = domainCategory.objects map (o => o -> transformPerObject(o)) toMap
  
  override lazy val hashCode: Int = f.hashCode | g.hashCode*17 | asMap.hashCode*31
  
  override def equals(x: Any): Boolean = x match {
    case other: NaturalTransformation[XObjects, XArrows, YObjects, YArrows] =>
      f == other.f && g == other.g && asMap == other.asMap
    case otherwise => false
  }
}

object NaturalTransformation {

  /**
    * Builds a identity natural transformation 1F: F -> F
    *
    * @tparam XObjects domain object type
    * @tparam XArrows domain arrow type
    * @tparam YObjects codomain object type
    * @tparam YArrows codomain arrow type
    * @param F the functor for which we are building the identity transformation
    * @return identity natural transformation for the functor
    */
  def id[XObjects, XArrows, YObjects, YArrows](
    F: Functor[XObjects, XArrows, YObjects, YArrows]):
  NaturalTransformation[XObjects, XArrows, YObjects, YArrows] = {

    def objectMap(x: XObjects): YArrows = F.codomain.id(F.nodesMapping(x))

    val transformPerObject = new SetMorphism[XObjects, YArrows](
      "id",
      F.domain.objects,
      F.codomain.arrows,
      objectMap)

    new NaturalTransformation[XObjects, XArrows, YObjects, YArrows](
      F, F, transformPerObject)
  }

}
