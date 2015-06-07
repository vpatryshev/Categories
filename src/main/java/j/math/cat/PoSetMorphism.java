package j.math.cat;

import java.util.Map;

/**
 * PoSetMorphism class: morphisms for posets.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 * @param <X> domain element type
 * @param <Y> codomain element type
 */
public abstract class PoSetMorphism<X, Y> extends SetMorphism<X, PoSet<X>, Y, PoSet<Y>> {

  /**
   * Constructor, pretty senseless. Builds a poset morphism that does not care about the order.
   * 
   * @param domain domain poset
   * @param codomain codomain poset
   */
  public PoSetMorphism(PoSet<X> domain, PoSet<Y> codomain) {
    super(domain, codomain);
    validate();
  }

  /**
   * Constructor, pretty senseless. Builds a poset morphism that does not care about the order.
   * 
   * @param id morphism id
   * @param domain domain poset
   * @param codomain codomain poset
   */
  public PoSetMorphism(String id, PoSet<X> domain, PoSet<Y> codomain) {
    super(id, domain, codomain);
    validate();
  }

  /**
   * Validates poset morphism, that is, checks that order is preserved
   */
  private void validate() {
    for (X x1 : domain()) for (X x2 : domain()) {
      if (domain()._le_(x1, x2)) {
        assert codomain()._le_(apply(x1), apply(x2));
      }
    }
  }

  @Override
  public String toString() {
    if (name() != null) return name();
    StringBuffer sb = new StringBuffer();
    for (X x : domain()) {
      if (sb.length() > 0) sb.append(", ");
      sb.append(x).append(" -> ").append(apply(x));
    }

    return "{" + sb + "}";
  }

  @Override
  public abstract Y apply(X x);

  /**
   * Factory method
   * 
   * @param <X> domain element type
   * @param <Y> codomain element type
   * @param domain domain poset
   * @param codomain codomain poset
   * @param map maps elements of domain to elements of codomain
   * @return the poset morphism
   */
  @SuppressWarnings("hiding")
  public <X, Y> PoSetMorphism<X, Y> PosetMorphism(final PoSet<X> domain, final PoSet<Y> codomain, final Map<X, Y> map) {
    for (X x : domain) {
      assert map.containsKey(x) : "Map should be defined on d0 element " + x;
    }

    return new PoSetMorphism<X, Y>(domain, codomain) {
      @Override
      public Y apply(X x) {
        assert domain.contains(x) : "Argument must be in d0";
        return map.get(x);
      }
    };
  }

  /**
   * Factory method. Builds unit morphism for a poset (identity function).
   * 
   * @param <X> element type 
   * @param s the poset
   * @return identity morphism on the given poset
   */
  public static <X> PoSetMorphism<X, X> unit(PoSet<X> s) {
    return new PoSetMorphism<X, X>(s, s) {
      @Override
      public X apply(X x) {
        return x;
      }
    };
  }

  /**
   * Composes two morphisms.
   * 
   * @param <X> domain element type for the first morphism (f)
   * @param <Y> domain element type for the second morphism (g)
   * @param <Z> codomain element type for the second morphism (g)
   * @param f first morphism
   * @param g second morphism
   * @return composition of f and g (f followed by g)
   */
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
      @Override
      public Z apply(X x) {
        return g.apply(f.apply(x));
      }
    };
  }
}
