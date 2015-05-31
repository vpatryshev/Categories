package math.cat;

import static math.cat.SetMorphism.Morphism;

import java.util.Set;
import java.util.Map;

/**
 * Functor class: morphisms for categories.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public class Functor<
    XObjects, // type of nodes in the first category (like Alksnis?)
    XArrows, // type of arrows in the first category
    YObjects, // type of nodes in the second category
    YArrows   // type of arrows in the second category
    > extends GraphMorphism<XObjects, XArrows, Category<XObjects, XArrows>, YObjects, YArrows, Category<YObjects, YArrows>> {

  /**
   * Constructor. Builds unnamed functor.
   *
   * @param domain domain category
   * @param codomain codomain category
   * @param objectsMorphism maps objects from the first category to the objects of the second category
   * @param arrowsMorphism maps arrows from the first category to the arrows of the second category
   */
  public Functor(
      Category<XObjects, XArrows> domain,
      Category<YObjects, YArrows> codomain,
      SetMorphism<XObjects, Set<XObjects>, YObjects, Set<YObjects>> objectsMorphism,
      SetMorphism<XArrows, Set<XArrows>, YArrows, Set<YArrows>> arrowsMorphism) {
    super(domain, codomain, objectsMorphism, arrowsMorphism);
    validate();
  }

  /**
   * Constructor. Builds named functor.
   *
   * @param name name of this functor
   * @param domain domain category
   * @param codomain codomain category
   * @param objectsMorphism maps objects from the first category to the objects of the second category
   * @param arrowsMorphism maps arrows from the first category to the arrows of the second category
   */
  public Functor(
      String name,
      Category<XObjects, XArrows> domain,
      Category<YObjects, YArrows> codomain,
      SetMorphism<XObjects, Set<XObjects>, YObjects, Set<YObjects>> objectsMorphism,
      SetMorphism<XArrows, Set<XArrows>, YArrows, Set<YArrows>> arrowsMorphism) {
    super(name, domain, codomain, objectsMorphism, arrowsMorphism);
    validate();
  }

  /**
   * Validates this functor.
   * A functor is valid if it is valid as a graph morphism, and besides,
   * it preserves unit and arrows composition.
   * That is, F(unit(x)) == unit(F(x)), and
   * F(g) o F(f) = F(g o f)
   */
  private void validate() {
    for (XObjects x : domain().objects()) {
      XArrows ux = domain().unit(x);
      YObjects y = nodesMorphism.apply(x);
      YArrows uy = codomain().unit(y);
      assert uy.equals(arrowsMorphism.apply(ux)) :
          "Functor must preserve units (failed on " + x + ")";
    }

    for (XArrows fx : domain().arrows()) for (XArrows gx : domain().arrows()) {
      if (domain().d1(fx).equals(domain().d0(gx))) {
        XArrows gx_fx = domain().m(fx, gx);
        YArrows fy = arrowsMorphism.apply(fx);
        YArrows gy = arrowsMorphism.apply(gx);
        YArrows gy_fy = codomain().m(fy, gy);
        assert gy_fy.equals(arrowsMorphism.apply(gx_fx)) :
            "Functor must preserve composition (failed on " + fx + ", " + fy + ")";
      }
    }
  }


  /**
   * Factory method. Builds unit functor for a category (identity functor).
   *
   * @param c the category
   * @return identity functor on the given category
   */
  public static <XObjects, XArrows>
    Functor<XObjects, XArrows, XObjects, XArrows> unit(Category<XObjects, XArrows> c) {
    return new Functor<XObjects, XArrows, XObjects, XArrows>
        (c, c, SetMorphism.unit(c.objects()), SetMorphism.unit(c.arrows()));
  }

  /**
   * Composes two functors
   * @param f : X -> Y - first functor
   * @param g : Y -> Z - second functor
   * @return g o f : X -> Z - their composition
   */
  public static <
      XObjects,  // nodes type for the category in the chain
      XArrows, // arrows type for the first category in the chain
      YObjects,  // nodes type for the second category in the chain
      YArrows, // arrows type for the second category in the chain
      ZObjects,  // nodes type for the third category in the chain
      ZArrows  // arrows type for the third category in the chain
     >
  Functor<XObjects, XArrows, ZObjects, ZArrows>
  compose(
      final Functor<XObjects, XArrows, YObjects, YArrows> f,
      final Functor<YObjects, YArrows, ZObjects, ZArrows> g
    ) {
    assert f.codomain().equals(g.domain()): "Composition not defined";
    return new Functor<XObjects, XArrows, ZObjects, ZArrows>(
        f.domain(),
        g.codomain(),
        SetMorphism.compose(f.nodesMorphism, g.nodesMorphism),
        SetMorphism.compose(f.arrowsMorphism, g.arrowsMorphism));
  }

  /**
   * Builds a functor, given two categories and two maps, one for the set of nodes, the other for the set of arrows.
   *
   * @param domain first category
   * @param codomain second category
   * @param nodesMap maps nodes of the first category to nodes of the second category
   * @param arrowsMap maps arrows of the first category to arrows of the second category
   * @return a functor that encapsluates all this
   */
  public static <
      // generic parameters
      XNodes, XArrows, // first category
      YNodes, YArrows  // second category
      >
  Functor<XNodes, XArrows, YNodes, YArrows> Functor(
      final Category<XNodes, XArrows> domain,
      final Category<YNodes, YArrows> codomain,
      final Map<XNodes, YNodes> nodesMap,
      final Map<XArrows, YArrows> arrowsMap) {

    return new Functor<XNodes, XArrows, YNodes, YArrows>(
        domain,
        codomain,
        Morphism(domain.nodes(), codomain.nodes(), nodesMap),
        Morphism(domain.arrows(), codomain.arrows(), arrowsMap));
  }
}
