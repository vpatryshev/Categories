package j.math.cat;

import static j.math.cat.Functions.forMap;

import j.math.cat.Functions.Function;
import j.math.cat.Functions.Inclusion;
import j.math.cat.Functions.Injection;
import j.math.cat.Functions.PairsToMap;

import java.util.Map;
import java.util.Set;

/**
 * SetMorphism class: morphisms for sets.
 * 
 * @param <X> domain element type
 * @param <PX> domain set type
 * @param <Y> codomain element type
 * @param <PY> codomain set type
 */
public abstract class SetMorphism <X, PX extends Set<X>, Y, PY extends Set<Y>> 
    extends Morphism<PX, PY> {

  /**
   * Applies the set morphism, that is a function, to an element of the set
   *
   * @param x element to apply the morphism to
   * @return morphism value on x
   */
  public abstract Y apply(X x);

  /**
   * Constructor.
   *
   * @param domain   domain of this morphism
   * @param codomain codomain of this morphism
   */
  public SetMorphism(PX domain, PY codomain) {
    super(domain, codomain);
    validate();
  }

  /**
   * Constructor.
   *
   * @param name     name of this morphism
   * @param domain   domain of this morphism
   * @param codomain codomain of this morphism
   */
  public SetMorphism(String name, PX domain, PY codomain) {
    super(name, domain, codomain);
    validate();
  }

  /**
   * Represents this morphism as a function.
   *
   * @return a function that does the same
   */
  public Function<X, Y> asFunction() {
    return new Function<X, Y>() {
      @Override
      public Y apply(X x) {
        return SetMorphism.this.apply(x);
      }
    };
  }

  /**
   * Validates set morphism.
   * All we need is that each domain element is mapped to a codomain element.
   */
  private void validate() {
    assert domain() != null : "Domain should not be null";
    assert codomain() != null : "Codomain should not be null";
    for (X x : domain()) {
      Y y = apply(x);
      assert (codomain().contains(y)) : "Morphism value " + y + " for " + x + " should be in codomain " + codomain();
    }
  }

  @Override
  public String toString() {
    if (name() != null) {
      return name();
    }
    StringBuffer sb = new StringBuffer();
    for (X x : domain()) {
      if (sb.length() > 0) {
        sb.append(", ");
      }
      sb.append(x).append(" -> ").append(apply(x));
    }

    return "{" + sb + "}";
  }

  @Override
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
      Y yThis = apply(x);
      Y yOther = other.apply(x);
      isEqual = isEqual && yThis.equals(yOther);
    }
    return isEqual;
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean equals(Object o) {
    return o instanceof SetMorphism && (((SetMorphism) o).equals(this));
  }

  /**
   * Factory method. Builds a set morphism, given two sets and a function.
   * 
   * @param <X> domain set element type 
   * @param <Y> codomain set element type
   * @param domain domain set
   * @param codomain codomain set
   * @param f maps domain elements to codomain elements
   * @return a new instance of SetMorphism
   */
  public static <X, Y> SetMorphism<X, Set<X>, Y, Set<Y>>
      Morphism(final Set<X> domain, final Set<Y> codomain, final Function<X, Y> f) {

    return new SetMorphism<X, Set<X>, Y, Set<Y>>(domain, codomain) {
      @Override
      public Y apply(X x) {
        return f.apply(x);
      }
    };
  }

  /**
   * Factory method. Builds a set morphism, given two sets and a map.
   * 
   * @param <X> domain set element type 
   * @param <Y> codomain set element type
   * @param domain domain set
   * @param codomain codomain set
   * @param map maps domain elements to codomain elements
   * @return a new instance of SetMorphism
   */
  public static <X, Y> SetMorphism<X, Set<X>, Y, Set<Y>>
      Morphism(final Set<X> domain, final Set<Y> codomain, final Map<X, Y> map) {
    for (X x : domain) {
      assert map.containsKey(x) : "Map should be defined on element " + x;
    }

    return new SetMorphism<X, Set<X>, Y, Set<Y>>(domain, codomain) {
      @Override
      public Y apply(X x) {
        assert domain.contains(x) : "Argument not in d0";
        return map.get(x);
      }
    };
  }

  /**
   * Factory method. Builds unit morphism for a set (identity function).
   *
   * @param <X> set element type
   * @param s the set
   * @return identity morphism on the given set
   */
  public static <X> SetMorphism<X, Set<X>, X, Set<X>> unit(Set<X> s) {
    return new SetMorphism<X, Set<X>, X, Set<X>>(s, s) {
      @Override
      public X apply(X x) {
        return x;
      }
    };
  }

  /**
   * Factory method. Builds constant morphism from a set that takes a constant value in another set (constant function).
   * 
   * @param <X> domain set element type
   * @param <Y> codomain set element type
   * @param px domain set
   * @param py codomain set
   * @param y  the value of the constant
   * @return constant morphism on the given set
   */
  public static <X, Y> SetMorphism<X, Set<X>, Y, Set<Y>> constant(Set<X> px, Set<Y> py, final Y y) {
    return new SetMorphism<X, Set<X>, Y, Set<Y>>(px, py) {
      @Override
      public Y apply(X x) {
        return y;
      }
    };
  }

  /**
   * Composes two set morphisms. Note that while SetMorphism can be subclassed to be used by morphisms of subclasses
   * of Set, composition cannot be defined in a generic way.
   * To make it possible in Java (with generics), one needs closures!
   * 
   * @param <X> domain set element type for morphism f
   * @param <Y> domain set element type for morphism g
   * @param <Z> codomain set element type for morphism g
   * @param f first morphism: X -> Y
   * @param g second morphism: Y -> Z
   * @return their composition g o f: X -> Z
   */
  public static <X, Y, Z> SetMorphism<X, Set<X>, Z, Set<Z>> 
      compose(
          final SetMorphism<X, Set<X>, Y, Set<Y>> f, // X -> Y
          final SetMorphism<Y, Set<Y>, Z, Set<Z>> g  // Y -> Z
  ) {
    assert f.codomain().equals(g.domain()) : "Composition not defined";
    return new SetMorphism<X, Set<X>, Z, Set<Z>>(f.domain(), g.codomain()) {
      @Override
      public Z apply(X x) {
        return g.apply(f.apply(x));
      }
    };
  }

  /**
   * Lists all possible set morphisms from Set x to Set y.
   * 
   * @param <X> domain set element type 
   * @param <Y> codomain set element type
   * @param setX domain of the morphisms
   * @param setY codomain of the morphisms
   * @return an iterable of all possible morphisms from one to another
   */
  @SuppressWarnings("unchecked")
  public static <X, Y> Iterable<SetMorphism<X, Set<X>, Y, Set<Y>>>
  hom(final Set<X> setX, final Set<Y> setY) {
    // For each x, iterates over Y
    Set<Set<Pair<X, Y>>> sections =
        new Injection<X, Set<Pair<X, Y>>>() {
          // this function builds pairs (x, y) for all y for a given x
          @Override
          public Set<Pair<X, Y>> apply(final X x) {
            Injection<Y, Pair<X, Y>> pairWithxOnTheLeft = BasePair.withLeft(x);
            return pairWithxOnTheLeft.map(setY);
          }
        }.map(setX);

    Set<Map<X, Y>> allMaps = new PairsToMap<X, Y>().map(Sets.Cartesian.product(sections));

    Function<Map<X, Y>, SetMorphism<X, Set<X>, Y, Set<Y>>> makeMorphism =
        new Function<Map<X, Y>, SetMorphism<X, Set<X>, Y, Set<Y>>>() {
          @Override
          public SetMorphism<X, Set<X>, Y, Set<Y>> apply(final Map<X, Y> map) {
            return Morphism(setX, setY, map);
          }
        };

    return makeMorphism.map(allMaps);
  }

  /**
   * Reverts this morphism, building another morphism from codomain
   * to the powerset of domain, mapping each element of codomain its reverse image.
   *
   * @return the new morphism.
   */
  public SetMorphism<Y, Set<Y>, Set<X>, Set<Set<X>>> revert() {
    Map<Y, Set<X>> reverse = Sets.groupBy(domain(), codomain(), asFunction());
    return Morphism(codomain(), Sets.powerset(domain()), forMap(reverse));
  }

  /**
   * Views this morphism as a map which keyset is domain, and mapping is defined by the
   * underlying function.
   *
   * @return the map
   */
  public Map<X, Y> asMap() {
    return asFunction().toMap(domain());
  }
}