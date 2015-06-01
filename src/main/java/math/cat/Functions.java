package math.cat;

import static math.cat.Sets.Map;
import static math.cat.Sets.Set;

import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Contains function objects and operations.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class Functions {

  private Functions() {}

  /**
   * Represents a function from type X to type Y.
   * Has additional functionality to make life easier.
   * @param <X> argument (domain) type
   * @param <Y> value (codomain) type
   */
  public abstract static class Function<X, Y> {
 
    /**
     * Produces function value for x.
     * @param x function argument
     * @return function value
     */
    public abstract Y apply(X x);

    /**
     * Builds a composition with another function.
     * Do not use it directly, use compose(f,g).
     *
     * @param g function that is applied after this one.
     * @return a composition of this function with g.
     */
    protected <Z> Function<X, Z> then(final Function<Y, Z> g) {
      final Function<X, Y> f = this;
      return new Function<X, Z>() {
        @Override
        public Z apply(X t) {
          return g.apply(f.apply(t));
        }
      };
    }

    /**
     * Maps an iterator, applying this function to its contents. Like list comprehension.
     * Mapping is lazy; the function value is recalculated for each argument when needed.
     *
     * @param domain the iterator to apply this function to.
     * @return another iterator that lists values of this function on domain values.
     */
    public Iterator<Y> map(final Iterator<? extends X> domain) {
      return new Iterator<Y>() {
        public boolean hasNext() {
          return domain.hasNext();
        }

        public Y next() {
          return apply(domain.next());
        }

        public void remove() {
          domain.remove();
        }
      };
    }

    /**
     * Maps a map, applying this function to its values. Like composition.
     * Mapping is lazy; the function value is recalculated for each argument when needed.
     * @param <T> map key type
     * @param map the map to which values to apply this function.
     * @return another map that has function values as values.
     */
    public <T> Map<T, Y> map(Map<T, X> map) {
      return forMap(map).then(this).toMap(map.keySet());
    }

    /**
     * Maps an iterable, applying this function to its contents. Like list comprehension.
     * Mapping is lazy; the function value is recalculated for each argument when needed.
     *
     * @param domain the iterable to apply this function to.
     * @return another iterable that lists values of this function on domain values.
     */
    public Iterable<Y> map(final Iterable<? extends X> domain) {
      return new Iterable<Y>() {
        public Iterator<Y> iterator() {
          return map(domain.iterator());
        }
      };
    }

    /**
     * Maps a collection, applying this function to its elements. Like list comprehension.
     * Mapping is lazy; the function value is recalculated for each argument when needed.
     *
     * @param domain the collection to apply this function to.
     * @return another collection that lists values of this function on domain values.
     */
    public Collection<Y> map(final Collection<? extends X> domain) {
      return new AbstractCollection<Y>() {
        @Override
        public Iterator<Y> iterator() {
          return map(domain.iterator());
        }

        @Override
        public int size() {
          return domain.size();
        }
      };
    }

    /**
     * Maps a list, applying this function to its elements. Like list comprehension.
     * Mapping is lazy; the function value is recalculated for each argument when needed.
     *
     * @param domain the list to apply this function to.
     * @return another list that lists values of this function on domain values.
     */
    public List<Y> map(final List<? extends X> domain) {
      return new AbstractList<Y>() {
        @Override
        public Y get(int index) {
          return apply(domain.get(index));
        }

        @Override
        public Iterator<Y> iterator() {
          return map(domain.iterator());
        }

        @Override
        public int size() {
          return domain.size();
        }
      };
    }

    /**
     * Turns a function into a map, by providing the set of keys (called 'domain' here).
     * Lazily evaluated.
     *
     * @param domain the set of keys.
     * @return a map that has a given set of keys and with values produced by applying
     *         the function to the keys.
     */
    public Map<X, Y> toMap(final Set<X> domain) {
      return Map(schwartzianTransform(this).map(domain));
    }
  }

  /**
   * Turns a map into a function.
   * @param <X> map key type
   * @param <Y> map value type
   * @param map the map that maps keys to values
   * @return a function that applies the map to its arguments.
   */
  public static <X, Y> Function<X, Y> forMap(final Map<X, Y> map) {
    return new Function<X, Y>() {

      @Override
      public Y apply(X x) {
        return map.get(x);
      }
    };
  }

  /**
   * Composes two function, producing another function that applies first f and then g.
   * 
   * @param <X> first function domain type
   * @param <Y> first function codomain type, same as second function domain type
   * @param <Z> second function codomain type
   * @param f first function
   * @param g second function
   * @return composition of the two functions
   */
  public static <X, Y, Z> Function<X, Z> compose(final Function<X, Y> f, final Function<Y, Z> g) {
    return f.then(g);
  }

  /**
   * Injection class.
   * Injection is a function that maps two different arguments to two different values.
   * What is good about an Injection is that when it maps a set, the resulting set
   * has the same number of elements.
   * @param <X> domain type
   * @param <Y> codomain type
   */
  public abstract static class Injection<X, Y> extends Function<X, Y> {

    /**
     * Composes this Injection with another function; if the other function
     * is also an Injection, the result is an Injection too.
     *
     * @param g another function
     * @return a composition
     *         TODO(vpatryshev): figure out how to use double dispatch here.
     */
    @Override
    protected <Z> Function<X, Z> then(final Function<Y, Z> g) {
      return g instanceof Injection ? then((Injection<Y, Z>) g) : super.then(g);
    }

    /**
     * Composes this Injection with another Injection.
     *
     * @param g another Injection
     * @return a composition
     */
    protected <Z> Function<X, Z> then(final Injection<Y, Z> g) {
      final Injection<X, Y> f = this;
      return new Injection<X, Z>() {
        @Override
        public Z apply(X t) {
          return g.apply(f.apply(t));
        }
      };
    }

    /**
     * Maps a set to another set using this function
     * @param domain set to map
     * @return a set of function values on this set, since the function is an injection, it's a set
     */
    public Set<Y> map(final Set<? extends X> domain) {
      return new AbstractSet<Y>() {
        @Override
        public Iterator<Y> iterator() {
          return map(domain.iterator());
        }

        @Override
        public int size() {
          return domain.size();
        }
      };
    }
  }

  /**
   * A special kind of injection that maps a set of key-value pairs to a map.
   *
   * @param <X> key type
   * @param <Y> value type
   */
  public static class PairsToMap<X, Y> extends Injection<Set<Pair<X, Y>>, Map<X, Y>> {
    @Override
    public Map<X, Y> apply(Set<Pair<X, Y>> pairs) {
      return Map(pairs);
    }
  }

  /**
   * A special kind of function that takes an Iterable and builds a set of its elements.
   *
   * @param <T> element type
   */
  public static class IterableToSet<T> extends Function<Iterable<T>, java.util.Set<T>> {
    @Override
    public Set<T> apply(Iterable<T> iterable) {
      return Set(iterable);
    }
  }

  /**
   * Injection of values of a certain type T1 to themselves, as a super type T.
   * @param <T> main type
   * @param <T1> subtype
   */
  public static class Inclusion<T, T1 extends T> extends Injection<T1, T> {
    @Override
    public T apply(T1 t) {
      return t;
    }
  }

  /**
   * Builds an inclusion of type T1 to type T
   * @param <T> main type
   * @param <T1> subtype
   * @return an inclusion function that maps every instance of T1 to itself, considered as T
   */
  public static <T, T1 extends T> Inclusion<T, T1> inclusion() {
    return new Inclusion<T, T1>();
  }

  /**
   * Isomorphic function. Meaning, it has a revert, unapply(), such that
   * apply(unapply(y)) == y, and unapply(apply(x)) == x.
   * 
   * @param <X> domain type
   * @param <Y> codomain type
   */
  public abstract static class Bijection<X, Y> extends Injection<X, Y> {
    /**
     * Operation that is inverse to apply: apply(unapply(y)) == y and unapply(apply(x)) == x
     * @param y an argument
     * @return an x such that apply(x) == y
     */
    abstract public X unapply(Y y);

    /**
     * Composes this Bijection with another function; if the other function
     * is also a Bijection, the result is a Bijection too.
     *
     * @param g another function
     * @return a composition
     *         TODO(vpatryshev): figure out how to use double dispatch here.
     */
    @Override
    protected <Z> Function<X, Z> then(final Function<Y, Z> g) {
      return g instanceof Bijection ? then((Bijection<Y, Z>) g) : super.then(g);
    }

    /**
     * Composes this Bijection with another Bijection.
     *
     * @param g another bijection
     * @return a composition
     */
    protected <Z> Function<X, Z> then(final Bijection<Y, Z> g) {
      final Bijection<X, Y> f = this;
      return new Bijection<X, Z>() {
        @Override
        public Z apply(X t) {
          return g.apply(f.apply(t));
        }

        @Override
        public X unapply(Z z) {
          return f.unapply(g.unapply(z));
        }
      };
    }
  }

  /**
   * Identity isomporphism on type T.
   * @param <T> domain type
   */
  public static class Id<T> extends Bijection<T, T> {
    @Override
    public T apply(T t) {
      return t;
    }

    @Override
    public T unapply(T t) {
      return t;
    }
  }

  /**
   * Builds a (virtual) identity map from a set to itself.
   * @param <T> domain type
   *
   * @param set set on which identity map is defined
   * @return the identity map
   */
  public static <T> Map<T, T> id(final Set<T> set) {
    return new Id<T>().toMap(set);
  }

  /**
   * Given a function f, builds another function that for each x
   * builds Map.Entry(x, f(x))
   * 
   * @param <X> domain type
   * @param <Y> codomain type
   * @param f function to apply
   * @return a function that builds pairs.
   */
  public static <X, Y> Bijection<X, Pair<X, Y>> schwartzianTransform(final Function<X, Y> f) {
    return new Bijection<X, Pair<X, Y>>() {
      @Override
      public Pair<X, Y> apply(final X x) {
        return math.cat.LazyPair.LazyPair(x, f);
      }

      @Override
      public X unapply(Pair<X, Y> p) {
        return p.x();
      }
    };
  }

  /**
   * Builds a function that, returns list element by its index.
   * 
   * @param <X> list element type
   * @param list the list
   * @return the function
   */
  public static <X> Function<Integer, X> forList(final List<X> list) {
    return new Function<Integer, X>() {

      @Override
      public X apply(Integer i) {
        return list.get(i);
      }};
  }

  /**
   * Builds constant function
   * @param <X> domain type
   * @param <Y> codomain type
   * @param value the only value it ever returns
   * @return the constant function
   */
  public static <X, Y> Function<X, Y> constant(final Y value) {
    return new Function<X, Y>() {

      @Override
      public Y apply(X x) {
        return value;
      }};
  }
}
