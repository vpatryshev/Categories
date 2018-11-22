package math.cat

/**
  * Natural transformation class: morphisms for functors.
  *
  * @tparam XObjects functors domain object type
  * @tparam XArrows functors domain arrow type
  * @tparam YObjects functors codomain object type
  * @tparam YArrows functors codomain arrow type
  * @param F        first functor
  * @param G        second functor
  * @param transformPerObject a set morphism that for each domain object x returns F(x) -> G(x)
  */
class NaturalTransformation[
    XObjects, // type of nodes in the first category (like Alksnis?) 
    XArrows, // type of arrows in the first category  
    YObjects, // type of nodes in the second category 
    YArrows // type of arrows in the second category   
  ](
    F: Functor[XObjects, XArrows, YObjects, YArrows],
    G: Functor[XObjects, XArrows, YObjects, YArrows],
    val transformPerObject: SetMorphism[XObjects, YArrows]
 ) extends Morphism[
  Functor[XObjects, XArrows, YObjects, YArrows],
  Functor[XObjects, XArrows, YObjects, YArrows]] {
  val d0: Functor[XObjects, XArrows, YObjects, YArrows] = F
  val d1: Functor[XObjects, XArrows, YObjects, YArrows] = G

  def compose(
    next: NaturalTransformation[XObjects, XArrows, YObjects, YArrows]
  ): NaturalTransformation[XObjects, XArrows, YObjects, YArrows] = {
    val newTag = next.transformPerObject.tag + " o " + transformPerObject.tag
    val targetCategory = d1.codomain
    def comp(x: XObjects) =
      targetCategory.m(transformPerObject(x), next.transformPerObject(x)).
        getOrElse(throw new IllegalArgumentException(s"Bad transformation for $x"))
    
    val trans = new SetMorphism[XObjects, YArrows](
      newTag, transformPerObject.d0, transformPerObject.d1, comp)
    
    new NaturalTransformation[XObjects, XArrows, YObjects, YArrows](F, next.d1, trans)
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

    def objectMap(x: XObjects): YArrows = F.codomain.id(F.nodesMorphism(x))

    val transformPerObject = new SetMorphism[XObjects, YArrows](
      "id",
      F.domain.objects,
      F.codomain.arrows,
      objectMap)

    new NaturalTransformation[XObjects, XArrows, YObjects, YArrows](
      F, F, transformPerObject)
  }

}
