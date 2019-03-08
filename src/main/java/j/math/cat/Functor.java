package j.math.cat;

import j.math.cat.Functions.Injection;
import j.math.cat.Functions.IterableToSet;
import j.math.cat.Functions.PairsToMap;

import java.util.Map;
import java.util.Set;

import static j.math.cat.Base.require;
import static j.math.cat.SetMorphism.Morphism;
import static j.math.cat.Sets.Set;

/**
 * Functor class: morphisms for categories.
 *
 * @param <XObjects> type of nodes in the first category (like Alksnis?)
 * @param <XArrows>  type of arrows in the first category
 * @param <YObjects> type of nodes in the second category
 * @param <YArrows>  type of arrows in the second category
 *
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 */
public class Functor<
    XObjects,
    XArrows,
    YObjects,
    YArrows
    > extends GraphMorphism<XObjects, XArrows, Category<XObjects, XArrows>, YObjects, YArrows, Category<YObjects, YArrows>> {

  /**
   * Constructor. Builds unnamed functor.
   *
   * @param domain          domain category
   * @param codomain        codomain category
   * @param objectsMorphism maps objects from the first category to the objects of the second category
   * @param arrowsMorphism  maps arrows from the first category to the arrows of the second category
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
   * @param name            name of this functor
   * @param domain          domain category
   * @param codomain        codomain category
   * @param objectsMorphism maps objects from the first category to the objects of the second category
   * @param arrowsMorphism  maps arrows from the first category to the arrows of the second category
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
  protected void validate() {
    for (XObjects x : domain().objects()) {
      XArrows ux = domain().unit(x);
      YObjects y = nodesMorphism.apply(x);
      YArrows uy = codomain().unit(y);
      assert uy.equals(arrowsMorphism.apply(ux)) :
          "Functor must preserve units (failed on " + x + ")";
    }

    for (XArrows fx : domain().arrows()) {
      for (XArrows gx : domain().arrows()) {
        if (domain().d1(fx).equals(domain().d0(gx))) {
          XArrows gx_fx = domain().m(fx, gx);
          YArrows fy = arrowsMorphism.apply(fx);
          YArrows gy = arrowsMorphism.apply(gx);
          YArrows gy_fy = codomain().m(fy, gy);
          if (!gy_fy.equals(arrowsMorphism.apply(gx_fx))) {
            gy_fy.equals(arrowsMorphism.apply(gx_fx));
          }
          assert gy_fy.equals(arrowsMorphism.apply(gx_fx)) :
              "Functor must preserve composition (failed on " + fx + ", " + fy + ", " + gx + ", " + gy + ", " + gy_fy + ", " + arrowsMorphism.apply(gx_fx) + ")";
        }
      }
    }
  }

  /**
   * Factory method. Builds unit functor for a category (identity functor).
   *
   * @param <XObjects> type of domain objects
   * @param <XArrows>  type of domain arrows
   * @param c          the category
   * @return identity functor on the given category
   */
  public static <XObjects, XArrows>
  Functor<XObjects, XArrows, XObjects, XArrows> unit(Category<XObjects, XArrows> c) {
    return new Functor<XObjects, XArrows, XObjects, XArrows>
        (c, c, SetMorphism.unit(c.objects()), SetMorphism.unit(c.arrows()));
  }

  /**
   * Factory method. Builds constant functor from a category to an object in another.
   *
   * @param <XObjects> type of objects in the first category
   * @param <XArrows>  type of arrows in the first category
   * @param <YObjects> type of objects in the second category
   * @param <YArrows>  type of arrows in the second category
   * @param X          the category
   * @param Y          another category
   * @param y          an object in category Y
   * @return constant functor on X that takes maps all objects to y and all arrows to y's unit.
   */
  public static <XObjects, XArrows, YObjects, YArrows>
  Functor<XObjects, XArrows, YObjects, YArrows>
  constant(Category<XObjects, XArrows> X,
           Category<YObjects, YArrows> Y,
           YObjects y) {
    return new Functor<XObjects, XArrows, YObjects, YArrows>
        (X, Y, SetMorphism.constant(X.objects(), Y.objects(), y),
            SetMorphism.constant(X.arrows(), Y.arrows(), Y.unit(y)));
  }

  /**
   * Composes two functors
   *
   * @param <XObjects> nodes type for the category in the chain
   * @param <XArrows>  arrows type for the first category in the chain
   * @param <YObjects> nodes type for the second category in the chain
   * @param <YArrows>  arrows type for the second category in the chain
   * @param <ZObjects> nodes type for the third category in the chain
   * @param <ZArrows>  arrows type for the third category in the chain
   * @param f          : X -> Y - first functor
   * @param g          : Y -> Z - second functor
   * @return g o f : X -> Z - their composition
   */
  public static <
      XObjects,
      XArrows,
      YObjects,
      YArrows,
      ZObjects,
      ZArrows
      >
  Functor<XObjects, XArrows, ZObjects, ZArrows>
  compose(
      final Functor<XObjects, XArrows, YObjects, YArrows> f,
      final Functor<YObjects, YArrows, ZObjects, ZArrows> g
  ) {
    assert f.codomain().equals(g.domain()) : "Composition not defined";
    return new Functor<XObjects, XArrows, ZObjects, ZArrows>(
        f.domain(),
        g.codomain(),
        SetMorphism.compose(f.nodesMorphism, g.nodesMorphism),
        SetMorphism.compose(f.arrowsMorphism, g.arrowsMorphism));
  }

  /**
   * Builds a functor, given two categories and two maps, one for the set of nodes, the other for the set of arrows.
   *
   * @param <XObjects> first category node type
   * @param <XArrows>  first category arrow type
   * @param <YObjects> second category node type
   * @param <YArrows>  second category arrow type
   * @param domain     first category
   * @param codomain   second category
   * @param objectsMap maps nodes of the first category to nodes of the second category
   * @param arrowsMap  maps arrows of the first category to arrows of the second category
   * @return a functor that encapsulates all this
   */
  public static <
      XObjects, XArrows,
      YObjects, YArrows
      >
  Functor<XObjects, XArrows, YObjects, YArrows> Functor(
      final Category<XObjects, XArrows> domain,
      final Category<YObjects, YArrows> codomain,
      final Map<XObjects, YObjects> objectsMap,
      final Map<XArrows, YArrows> arrowsMap) {

    return new Functor<XObjects, XArrows, YObjects, YArrows>(
        domain,
        codomain,
        Morphism(domain.objects(), codomain.objects(), objectsMap),
        Morphism(domain.arrows(), codomain.arrows(), arrowsMap));
  }

  /**
   * Cone class for this Functor. A cone is an object y (called apex) and a bundle of arrows cx: y -> F(x)
   * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
   */
  public class Cone {
    /**
     * Apex of this cone
     */
    protected YObjects apex;

    private final Map<XObjects, YArrows> map;

    /**
     * Constructor
     *
     * @param apex cone apex object
     * @param map  maps domain objects to cone arrow components
     */
    public Cone(YObjects apex, Map<XObjects, YArrows> map) {
      this.apex = apex;
      this.map = map;
      assert apex != null : "an apex of a cone can't be null";
      assert map != null : "a map of arrows of a cone can't be null";
    }

    /**
     * @return apex, the object from which all the arrows of the cone originate.
     */
    public YObjects apex() {
      return apex;
    }

    /**
     * Given an object x of category X, returns an arrow y -> F(x) of this cone.
     *
     * @param x an object in X
     * @return the arrow
     */
    public YArrows arrowTo(XObjects x) {
      return map.get(x);
    }

    /**
     * @return true if this actually a well-formed cone.
     */
    public boolean isWellFormed() {
      return new Predicate<XArrows>() {
        // take an arrow f: x0 -> x1
        @Override
        public boolean eval(XArrows f) {
          // an arrow p0: y -> F(x0)
          YArrows yToFx0 = arrowTo(domain().d0(f));
          // an arrow p1: y -> F(x1)
          YArrows yToFx1 = arrowTo(domain().d1(f));
          // F(f)
          YArrows F_f = arrowsMorphism.apply(f);
          // F(f) o p0 must be equal to p1
          return codomain().m(yToFx0, F_f).equals(yToFx1);
        } // this should hold for all arrows f of category X
      }.forall(domain().arrows());
    }

    /**
     * A cone from y1 to F is factored by this cone (with apex y)
     * if there is an h : y1 -> y such that each f1: y1 -> F(x) is equal to
     * f o h, where f: y -> F(x).
     *
     * @param factored a cone that may be factored
     * @return true if it is so
     */
    boolean factorsOnRight(final Cone factored) {
      return new Predicate<YArrows>() {
        @Override
        public boolean eval(final YArrows h) {
          return new Predicate<XObjects>() {
            @Override
            public boolean eval(XObjects x) {
              return codomain().m(h, arrowTo(x)).equals(factored.arrowTo(x));
            }
          }.forall(domain().objects());
        }
      }.exists(codomain().arrows(factored.apex(), apex()));
    }

    @Override
    @SuppressWarnings({"rawtypes", "unchecked"})
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof Functor.Cone)) return false;

      Cone other = (Cone) o;
      if (!apex().equals(other.apex())) return false;
      for (XObjects x : domain()) {
        if (!Category.equal(arrowTo(x), other.arrowTo(x))) return false;
      }
      return true;
    }

    @Override
    public int hashCode() {
      int code = apex().hashCode();
      for (XObjects x : domain()) {
        code = code * 13 + arrowTo(x).hashCode();
      }
      return code;
    }

    @Override
    public String toString() {
      return "Cone[" + apex() + "]";
    }
  }

  /**
   * @return this functor's limit
   */
  public Cone limit() {
    return new Predicate<Cone>() {
      @Override
      public boolean eval(Cone candidate) {
        return isLimit(candidate);
      }
    }.findOne(allCones());
  }

  /**
   * @return all possible cones to this functor.
   */
  Set<Cone> allCones() {
    Injection<YObjects, Set<Cone>> buildAllConesFrom =
        new Injection<YObjects, Set<Cone>>() {
          // For an object y returns all cones from y to F.
          @Override
          public Set<Cone> apply(final YObjects y) {
            return conesFrom(y);
          }
        };
    return Sets.union(buildAllConesFrom.map(codomain().objects()));
  }

  /**
   * Builds a predicate that checks whether a given map constitutes a cone from an object to this functor.
   */
  private final Predicate<Cone> isaCone = new Predicate<Cone>() {
    @Override
    public boolean eval(Cone candidate) {
      return candidate.isWellFormed();
    }
  };

  /**
   * Lists all possible cones from given object y to this functor.
   * The requirement is that if f1: y -> F(x1) is in the collection of arrows,
   * and there is a g: x1 -> x2, then f2 = F(g) o f1 : y -> F(x2) is also in this collection.
   *
   * @param y an object from which the cone originates.
   * @return a map that maps objects x of domain category to arrows y -> F(x)
   */
  Set<Cone> conesFrom(final YObjects y) {
    final Injection<XObjects, Set<Pair<XObjects, YArrows>>> arrowsFromYtoFX =
        new Injection<XObjects, Set<Pair<XObjects, YArrows>>>() {
          // this function builds pairs (x, f:y->F(x)) for all f:y->F(x)) for a given x
          @Override
          public Set<Pair<XObjects, YArrows>> apply(final XObjects x) {
            return arrowsToFX(x);
          }

          private Set<Pair<XObjects, YArrows>> arrowsToFX(XObjects x) {
            return BasePair.<XObjects, YArrows>withLeft(x).
                map(codomain().arrows(y, nodesMorphism.apply(x)));
          }
        };

    final Set<XObjects> xs = domain().objects();
    // group (x, f: y->F[x]) by x
    Set<Set<Pair<XObjects, YArrows>>> homsGroupedByX
        = arrowsFromYtoFX.map(xs);

    // all possible combinations of {(x, y->F[x]) | x \in X}
    // that is, each element defines a selection of arrows from y - not all of them are cones
    Set<? extends Iterable<Pair<XObjects, YArrows>>> productOfHoms = Sets.Cartesian.product(homsGroupedByX);

    // the same, but now a set
    Set<Set<Pair<XObjects, YArrows>>> setOfSetsOfPairs =
        Set(new IterableToSet<Pair<XObjects, YArrows>>().map(productOfHoms));

    // the same, but now there's a map inside: for each x find an arrow, y->F[x]
    Set<Map<XObjects, YArrows>> xWithArrowEndingAtIt
        = new PairsToMap<XObjects, YArrows>().map(setOfSetsOfPairs);

    // builds a cone from a map x \to (y->F[x]) - not all of them are cones actually
    Injection<Map<XObjects, YArrows>, Cone> makeCone =
        new Injection<Map<XObjects, YArrows>, Cone>() {
          @Override
          public Cone apply(Map<XObjects, YArrows> map) {
            return new Cone(y, map);
          }
        };

    // out of all cone candidates select those that are actually cones
    return isaCone.filter(makeCone.map(xWithArrowEndingAtIt));
  }

  /**
   * Checks if a given Cone is a limit
   *
   * @param candidate cone to check
   * @return true iff it is a limit
   */
  boolean isLimit(final Cone candidate) {
    // a candidate is a limit if for each cone to F starting at y
    // the cone is factored through y0; and this is true for all y.
    return new Predicate<Cone>() {
      @Override
      public boolean eval(Cone anyCone) {
        return candidate.factorsOnRight(anyCone);
      }
    }.forall(allCones());
  }

  /**
   * Cocone class. Implements category's cocone. Consists of an apex object
   * and a bunch of arrows to the apex object from functor image objects.
   */
  public class Cocone extends BasePair<YObjects, Map<XObjects, YArrows>> {

    /**
     * Constructor.
     *
     * @param apex cocone apex
     * @param map  maps domain objects to arrows to apex
     */
    public Cocone(YObjects apex, Map<XObjects, YArrows> map) {
      super(apex, map);
    }

    /**
     * @return the object at which all the arrows of the cocone terminate.
     */
    public YObjects apex() {
      return x();
    }

    /**
     * Given an object x of category X, returns an arrow F(x) -> y of this cocone.
     *
     * @param x an object in X
     * @return the arrow
     */
    public YArrows arrowFrom(XObjects x) {
      return y().get(x);
    }

    /**
     * @return true if this actually a well-formed cocone.
     */
    public boolean isWellFormed() {
      return new Predicate<XArrows>() {
        // take an arrow f: x0 -> x1
        @Override
        public boolean eval(XArrows f) {
          // an arrow q0: F(x0) -> y
          YArrows Fx0ToY = arrowFrom(domain().d0(f));
          // an arrow q1: F(x1) -> y
          YArrows Fx1ToY = arrowFrom(domain().d1(f));
          // F(f)
          YArrows F_f = arrowsMorphism.apply(f);
          // q1 o F(f) must be equal to q0
          codomain().validate();
          require(codomain().arrows().contains(F_f), "Function should be in Y: " + F_f);
          final YArrows composition = codomain().m(F_f, Fx1ToY);
          require(composition != null, "Bad category, composition of " + F_f + " and " + Fx1ToY + " should be defined " + codomain().d1(F_f) + " vs " + codomain().d1(Fx1ToY));
          return composition.equals(Fx0ToY);
        } // this should hold for all arrows f of category X
      }.forall(domain().arrows());
    }

    /**
     * A cocone from F to y1 is factored by this cocone (with apex y)
     * if there is an h : y -> y1 such that each f1: F(x) -> y1 is equal to
     * h o f, where f: F(x) -> y.
     *
     * @param factored a cone that may be factored
     * @return true if it is so
     */
    boolean factorsOnLeft(final Cocone factored) {
      return new Predicate<YArrows>() {
        @Override
        public boolean eval(final YArrows h) {
          return new Predicate<XObjects>() {
            @Override
            public boolean eval(XObjects x) {
              return codomain().m(arrowFrom(x), h).equals(factored.arrowFrom(x));
            }
          }.forall(domain().objects());
        }
      }.exists(codomain().arrows(apex(), factored.apex()));
    }

    @Override
    @SuppressWarnings({"unchecked"})
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof Functor.Cocone)) return false;

      Cocone other = (Cocone) o;
      if (!apex().equals(other.apex())) return false;
      for (XObjects x : domain()) {
        if (!Category.equal(arrowFrom(x), other.arrowFrom(x))) return false;
      }
      return true;
    }

    @Override
    public int hashCode() {
      int code = apex().hashCode();
      for (XObjects x : domain()) {
        code = code * 13 + arrowFrom(x).hashCode();
      }
      return code;
    }

    @Override
    public String toString() {
      return "Cocone[" + apex() + "]";
    }
  }

  /**
   * @return a colimit of this functor.
   */
  public Cocone colimit() {
    return new Predicate<Cocone>() {
      @Override
      public boolean eval(Cocone candidate) {
        return isColimit(candidate);
      }
    }.findOne(allCocones());
  }

  /**
   * Checks if a cocone is a colimit
   *
   * @param candidate a candidate cocone
   * @return true iff the cocone is a colimit
   */
  public boolean isColimit(final Cocone candidate) {
    // a candidate is a colimit if for each cone from F ending at y
    // the cocone is factored through y0; and this is true for all y.
    return new Predicate<Cocone>() {
      @Override
      public boolean eval(Cocone anyCocone) {
        return candidate.factorsOnLeft(anyCocone);
      }
    }.forall(allCocones());
  }

  /**
   * @return all possible cocones for this functor.
   */
  Set<Cocone> allCocones() {
    Injection<YObjects, Set<Cocone>> buildAllCoconesTo =
        new Injection<YObjects, Set<Cocone>>() {
          // For an object y returns all cones from y to F.
          @Override
          public Set<Cocone> apply(final YObjects y) {
            return coconesTo(y);
          }
        };
    return Sets.union(buildAllCoconesTo.map(codomain().objects()));
  }

  /**
   * Lists all possible cocones from this functor to a given object y.
   * The requirement is that if f1: F(x1) -> y is in the collection of arrows,
   * and there is a g: x0 -> x1, then f0 = f1 o F(g) F(x0) -> y is also in this collection.
   *
   * @param y an object at which the cone terminates.
   * @return a map that maps objects x of domain category to arrows F(x) -> y
   */
  Set<Cocone> coconesTo(final YObjects y) {
    // For each x, iterates over hom(y, F(x))
    Set<Set<Pair<XObjects, YArrows>>> homs =
        new Injection<XObjects, Set<Pair<XObjects, YArrows>>>() {
          // this function builds pairs (x, f:F(x)->y) for all f:F(x)->y) for a given x
          @Override
          public Set<Pair<XObjects, YArrows>> apply(final XObjects x) {
            return BasePair.<XObjects, YArrows>withLeft(x).
                map(codomain().arrows(nodesMorphism.apply(x), y));
          }
        }.map(domain().objects());

    Set<? extends Iterable<Pair<XObjects, YArrows>>> productOfHoms = Sets.Cartesian.product(homs);
    Set<Set<Pair<XObjects, YArrows>>> setOfSetsOfPairs =
        Set(new IterableToSet<Pair<XObjects, YArrows>>().map(productOfHoms));

    Set<Map<XObjects, YArrows>> allMaps = new PairsToMap<XObjects, YArrows>().map(
        setOfSetsOfPairs);

    Injection<Map<XObjects, YArrows>, Cocone> makeCocone =
        new Injection<Map<XObjects, YArrows>, Cocone>() {
          @Override
          public Cocone apply(Map<XObjects, YArrows> map) {
            return new Cocone(y, map);
          }
        };

    return isaCocone.filter(makeCocone.map(allMaps));
  }

  /**
   * Builds a predicate that checks whether a given map constitutes a cocone from this functor to an object.
   */
  private final Predicate<Cocone> isaCocone = new Predicate<Cocone>() {
    @Override
    public boolean eval(Cocone candidate) {
      return candidate.isWellFormed();
    }
  };
}
