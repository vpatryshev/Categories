package math.cat;

import java.util.*;

/**
 * SetMorphism class: morphisms for sets.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public abstract class SetMorphism
    <X, // domain data type
     PX extends Set<X>, // domain type
     Y, // codomain data type
     PY extends Set<Y> // codomain type
    > extends Morphism<PX, PY> {

  /**
   * Constructor.
   *
   * @param domain domain of this morphism
   * @param codomain codomain of this morphism
   */
  public SetMorphism(PX domain, PY codomain) {
    super(domain, codomain);
    validate();
  }

  /**
   * Constructor.
   *
   * @param name name of this morphism
   * @param domain domain of this morphism
   * @param codomain codomain of this morphism
   */
  public SetMorphism(String name, PX domain, PY codomain) {
    super(name, domain, codomain);
    validate();
  }

  /**
   * Validates set morphism.
   * All we need is that, each domain element is mapped to a codomain element.
   */
  private void validate() {
    for (X x : domain()) assert(codomain().contains(apply(x))) : "Morphism value for " + x + " out of d1";
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

  /**
   * Applies the set morphism, that is a function, to an element of the set
   * @param x element to apply the morphism to
   * @return morphism value on x
   */
  public abstract Y apply(X x);

  public int hashCode() {
    return name() != null ? name().hashCode() : (domain().hashCode() * 4/*random number*/ + codomain().hashCode());
  }

  /**
   * Two set morphisms are equal if they have equal domains and codomains and map domain elements to the same values.
   * Note that there's no negation in this calculation; there is a deep reason for it, to be disclosed much, much later.
   * 
   * @param other set morphism to compare
   * @return true if they are equal
   */
  public boolean equals(SetMorphism<X, PX, Y, PY> other) {
    boolean isEqual = domain().equals(other.domain()) &&
                      codomain().equals(other.codomain());
    for (X x : domain()) {
      isEqual = isEqual && apply(x).equals(other.apply(x));
    }
    return isEqual;
  }

  public boolean equals(Object o) { return o instanceof SetMorphism && (((SetMorphism)o).equals(this)); }

  /**
   * Factory method. Builds a set morphism, given two sets and a map.
   *
   * @param domain domain set
   * @param codomain codomain set
   * @param map maps domain elements to codomain elements
   * @return a new instance of SetMorphism
   */
  public static <X, Y>
  SetMorphism<X, Set<X>, Y, Set<Y>> Morphism(final Set<X> domain, final Set<Y> codomain, final Map<X, Y> map) {
    for (X x : domain) {
      assert map.containsKey(x) : "Map should be defined on element " + x;
    }

    return new SetMorphism<X, Set<X>, Y, Set<Y>>(domain, codomain) {
      public Y apply(X x) {
        assert domain.contains(x) : "Argument not in d0";
        return map.get(x);
      }
    };
  }

  /**
   * Factory method. Builds unit morphism for a set (identity function).
   *
   * @param s the set
   * @return identity morphism on the given set
   */
  public static <X> SetMorphism<X, Set<X>, X, Set<X>> unit(Set<X> s) {
    return new SetMorphism<X, Set<X>, X, Set<X>>(s, s) {
      public X apply(X x) {
        return x;
      }
    };
  }

  /**
   * Composes two set morphisms. Note that while SetMorphism can be subclassed to be used by morphisms of subclasses
   * of Set, composition cannot be defined in a generic way.
   * To make it possible in Java (with generics), one needs closures!
   *
   * @param f first morphism: X -> Y
   * @param g second morphism: Y -> Z
   * @return their composition g o f: X -> Z
   */
  public static <
      X, // domain data type for the first morphism
      Y, // codomain data type for the first morphism (same as domain type for the second morphism)
      Z  // codomain data type for the first morphism
     >
  SetMorphism<X, Set<X>, Z, Set<Z>> compose(
      final SetMorphism<X, Set<X>, Y, Set<Y>> f, // X -> Y
      final SetMorphism<Y, Set<Y>, Z, Set<Z>> g  // Y -> Z
    ) {
    assert f.codomain().equals(g.domain()): "Composition not defined";
    return new SetMorphism<X, Set<X>, Z, Set<Z>>(f.domain(), g.codomain()) {
      public Z apply(X x) {
        return g.apply(f.apply(x));
      }
    };
  }
}