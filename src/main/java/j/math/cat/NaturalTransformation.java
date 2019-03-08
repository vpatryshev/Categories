package j.math.cat;

import java.util.Set;

/**
 * Natural transformation class: morphisms for functors.
 * 
 *
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * 
 * @param <XObjects> functors domain object type
 * @param <XArrows> functors domain arrow type
 * @param <YObjects> functors codomain object type
 * @param <YArrows> functors codomain arrow type
 */
public class NaturalTransformation<
    XObjects, // type of nodes in the first category (like Alksnis?)
    XArrows,  // type of arrows in the first category
    YObjects, // type of nodes in the second category
    YArrows   // type of arrows in the second category
    >
    extends Morphism<
        Functor<XObjects, XArrows, YObjects, YArrows>,
        Functor<XObjects, XArrows, YObjects, YArrows>> {
  @SuppressWarnings("unused")
  private final SetMorphism<XObjects, Set<XObjects>, YArrows, Set<YArrows>> Fx_to_Gx;

  /**
   * Constructor. Builds a natural transformation from functor F to functor G
   * @param F first functor
   * @param G second functor
   * @param Fx_to_Gx a set morphism that for each domain object  x returns F(x) -> G(x)
   */
  public NaturalTransformation(Functor<XObjects, XArrows, YObjects, YArrows> F,
                               Functor<XObjects, XArrows, YObjects, YArrows> G,
                               SetMorphism<XObjects, Set<XObjects>, YArrows, Set<YArrows>> Fx_to_Gx) {
    super(F, G);
    this.Fx_to_Gx = Fx_to_Gx;
  }

  /**
   * Builds a unit natural transformation 1F: F -> F
   * @param <XObjects> domain object type
   * @param <XArrows> domain arrow type
   * @param <YObjects> codomain object type
   * @param <YArrows> codomain arrow type
   * @param F the functor for which we are building the unit transformation
   * @return unit (identity) natural transformation for the functor
   */
  public static <XObjects, XArrows, YObjects, YArrows>
  NaturalTransformation<
      XObjects,
      XArrows,
      YObjects,
      YArrows
      > unit(final Functor<XObjects, XArrows, YObjects, YArrows> F) {
    return new NaturalTransformation<
        XObjects,
        XArrows,
        YObjects,
        YArrows>(F, F,
        new SetMorphism<XObjects, Set<XObjects>, YArrows, Set<YArrows>>(
            F.domain().objects(), F.codomain().arrows()) {
          @Override
          public YArrows apply(XObjects x) {
            return F.codomain().unit(F.nodesMorphism.apply(x));
          }
        }
    );
  }
}
