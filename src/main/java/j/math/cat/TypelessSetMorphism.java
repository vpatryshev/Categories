package j.math.cat;

import j.math.cat.Functions.Function;
import j.math.cat.Functions.Injection;

import java.util.Map;
import java.util.Set;

/**
 * Set morphism for typeless sets.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public abstract class TypelessSetMorphism
    extends SetMorphism<Object, Set<Object>, Object, Set<Object>> {

  /**
   * Constructor
   * @param domain morphism domain (a set)
   * @param codomain morphism codomain (a set)
   */
  @SuppressWarnings("unchecked")
  public TypelessSetMorphism(Set domain, Set codomain) {
    super(domain, codomain);
  }

  /**
   * Constructor
   * @param name morphism name
   * @param domain morphism domain (a set)
   * @param codomain morphism codomain (a set)
   */
  @SuppressWarnings("unchecked")
  public TypelessSetMorphism(String name, Set domain, Set codomain) {
    super(name, domain, codomain);
  }

  /**
   * Factory method. Builds unit morphism for a set (identity function).
   *
   * @param s the set
   * @return identity morphism on the given set
   */
  @SuppressWarnings("unchecked")
  public static TypelessSetMorphism unitMorphism(Set s) {
    return forFunction(s, s, new Functions.Id());
  }

  /**
   * Composes this morphism with the next one.
   *
   * @param g second morphism: Y -> Z
   * @return their composition g o f: X -> Z
   */
  public TypelessSetMorphism then(TypelessSetMorphism g)
  {
    assert codomain().equals(g.domain()) : "Composition not defined";
    return TypelessSetMorphism.forFunction(domain(), g.codomain(), asFunction().then(g.asFunction()));
  }

  /**
   * @return a function that composes a  morphism with this morphism
   */
  public Function<TypelessSetMorphism, TypelessSetMorphism> followedBy() {
    return new Function<TypelessSetMorphism, TypelessSetMorphism>() {

      @Override
      public TypelessSetMorphism apply(TypelessSetMorphism g) {
        return TypelessSetMorphism.this.then(g);
      }
      
    };
  }

  /**
   * Composes a morphism with this morphism.
   *
   * @param g first morphism: T -> X
   * @return their composition f o g: T -> Y
   */
  public TypelessSetMorphism before(TypelessSetMorphism g)
  {
    assert domain().equals(g.codomain()) : "Composition not defined";
    return TypelessSetMorphism.forFunction(g.domain(), codomain(), g.asFunction().then(asFunction()));
  }

  /**
   * @return a function that composes this morphism with another morphism
   */
  public Function<TypelessSetMorphism, TypelessSetMorphism> after() {
    return new Function<TypelessSetMorphism, TypelessSetMorphism>() {

      @Override
      public TypelessSetMorphism apply(TypelessSetMorphism g) {
        return TypelessSetMorphism.this.before(g);
        }
        
      };
    }
  
  /**
   * Composes two morphisms.
   *
   * @param f first morphism: X -> Y
   * @param g second morphism: Y -> Z
   * @return their composition g o f: X -> Z
   */
  public static TypelessSetMorphism compose(TypelessSetMorphism f, TypelessSetMorphism g)
  {
    return f.then(g);
  }

  /**
   * Factory method. Builds constant morphism from one set to another (constant function).
   *
   * @param dom   domain
   * @param codom codomain
   * @param value the value in <code>codom</code> that the morphism returns
   * @return constant morphism
   */
  @SuppressWarnings("unchecked")
  public static TypelessSetMorphism constant(final Set dom, final Set codom, final Object value) {
    return TypelessSetMorphism.forFunction(dom, codom, Functions.constant(value));
  }

  /**
   * Factory method. Builds a morphism for a given domain, codomain and a function.
   *
   * @param domain morphism domain
   * @param codomain morphism codomain
   * @param f the function that defines the morphism action
   * @return the morphism
   */
  @SuppressWarnings("unchecked")
  public static TypelessSetMorphism forFunction(Set domain, Set codomain, final Function f) {
    return new TypelessSetMorphism(domain, codomain) {
      @Override
      public Object apply(Object x) {
        return f.apply(x);
      }
    };
  }

  /**
   * Factory method. Builds an inclusion monomorphism that injects one set to another.
   *
   * @param subset the subset
   * @param set    the set
   * @return inclusion monomorphism
   */
  @SuppressWarnings("unchecked")
  public static TypelessSetMorphism inclusion(Set subset, Set set) {
    assert set.containsAll(subset) : "It is not an inclusion if it is not a subset.";
    return forFunction(subset, set, Functions.inclusion());
  }

  /**
   * Factory method. Builds an inclusion monomorphism that injects one set to another.
   * Subset is defined by a predicate.
   *
   * @param set the set
   * @param predicate defines the condition for elements to be included in the subset
   * @return inclusion monomorphism
   */
  @SuppressWarnings("unchecked")
  public static TypelessSetMorphism inclusion(Set set, Predicate<?> predicate) {
    return inclusion(predicate.filter(set), set);
  }

  /**
   * Factory method. Builds a factorset epimorphism that projects a set to its factorset,
   * given a set and binary relationship. Factoring is done on the relationship's transitive closure.
   *
   * @param factorset the main set
   * @return factorset epimorphism
   */
  public static TypelessSetMorphism forFactorset(Sets.FactorSet<Object> factorset) {
    return forFunction(factorset.domain(), factorset.factorset(), factorset.asFunction());
  }

  /**
   * Factory method. Builds a factorset epimorphism that projects a set to its factorset,
   * given a set and binary relationship. Factoring is done on the relationship's transitive closure.
   * @param <T> set element type 
   *
   * @param set the main set
   * @param r binary relationship (not necessarily equivalence relationship) that determines factoring
   * @return factorset epimorphism
   */
  public static <T> TypelessSetMorphism factorset(Set<T> set, BinaryRelationship<T, T> r) {
    Sets.FactorSet<T> factorset = new Sets.FactorSet<T>(set, r);
    return forFunction(set, factorset.factorset(), factorset.asFunction());
  }

  /**
   * Builds a set of all morphisms from one set to another.
   *
   * @param x exponent (the set from which all morphisms are)
   * @param y base (the set to which all morphisms are)
   * @return y<sup>x, represented as a set of all morphisms.
   */
  @SuppressWarnings("unchecked")
  public static Set<TypelessSetMorphism> power(final Set x, final Set y) {
    return new Injection<Map, TypelessSetMorphism>() {

      @Override
      public TypelessSetMorphism apply(final Map map) {
        return new TypelessSetMorphism(x, y) {
          @Override
          public Object apply(Object o) {
            return map.get(o);
          }
        };
      }
    }.map(Sets.allMaps(x, y));
  }
}
