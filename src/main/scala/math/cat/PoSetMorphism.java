package math.cat;

import java.util.*;

/**
 * PoSetMorphism class: morphisms for posets.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public abstract class PoSetMorphism<X, Y> extends SetMorphism<X, PoSet<X>, Y, PoSet<Y>> {

  public PoSetMorphism(PoSet<X> domain, PoSet<Y> codomain) {
    super(domain, codomain);
    validate();
  }

  public PoSetMorphism(String id, PoSet<X> domain, PoSet<Y> codomain) {
    super(id, domain, codomain);
    validate();
  }

  private void validate() {
    for (X x1 : domain()) for (X x2 : domain()) {
      if (domain()._le_(x1, x2)) {
        assert codomain()._le_(apply(x1), apply(x2));
      }
    }
  }

  public String toString() {
    if (name() != null) return name();
    StringBuffer sb = new StringBuffer();
    for (X x : domain()) {
      if (sb.length() > 0) sb.append(", ");
      sb.append(x).append(" -> ").append(apply(x));
    }

    return "{" + sb + "}";
  }

  public abstract Y apply(X x);

  public <X, Y> PoSetMorphism<X, Y> PosetMorphism(final PoSet<X> domain, final PoSet<Y> codomain, final Map<X, Y> map) {
    for (X x : domain) {
      assert map.containsKey(x) : "Map should be defined on d0 element " + x;
    }

    return new PoSetMorphism<X, Y>(domain, codomain) {
      public Y apply(X x) {
        assert domain.contains(x) : "Argument not in d0";
        return map.get(x);
      }
    };
  }

  /**
   * Factory method. Builds unit morphism for a poset (identity function).
   *
   * @param s the poset
   * @return identity morphism on the given poset
   */
  public static <X> PoSetMorphism<X, X> unit(PoSet<X> s) {
    return new PoSetMorphism<X, X>(s, s) {
      public X apply(X x) {
        return x;
      }
    };
  }

  public static <
      X, // domain data type for the first morphism
      Y, // codomain data type for the first morphism (same as domain type for the second morphism)
      Z  // codomain data type for the first morphism
     >
  PoSetMorphism<X, Z> compose(
      final PoSetMorphism<X, Y> f,
      final PoSetMorphism<Y, Z> g
    ) {
    assert f.codomain().equals(g.domain()): "Composition not defined";
    return new PoSetMorphism<X, Z>(f.domain(), g.codomain()) {
      public Z apply(X x) {
        return g.apply(f.apply(x));
      }
    };
  }
}
