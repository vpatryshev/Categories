package j.math.cat;

import static j.math.cat.Base.oneOf;
import static j.math.cat.Sets.Set;

import java.util.AbstractSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * Java representation of predicate.
 * A predicate is a logical-valued function. Here in Java we use boolean for logical.
 * <p/>
 * A bunch of helpful methods apply predicates to sets, producing virtual derivative sets.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 * @param <X> predicate parameter type
 */
public abstract class Predicate<X> {
  /**
   * Evaluates the predicate. You have to define this method in your specific predicate.
   *
   * @param x the value at which the predicate is evaluated
   * @return predicate value
   */
  public abstract boolean eval(X x);

  private String id = null;

  /**
   * @return true iff we are tracing the execution of this predicate
   */
  protected boolean isTracing() {
    return id != null;
  }

  /**
   * Trace a predicate execution, with additional data
   * @param data data to log
   */
  protected void trace(Object... data) {
    if (isTracing()) {
      System.out.print(id);
      System.out.print(": ");
      for (Object o : data) {
        System.out.print(o);
      }
      System.out.println();
    }
  }

  /**
   * Starts tracing.
   *
   * @param id an identifier of this predicate
   * @return the same predicate
   */
  public Predicate<X> startTracing(@SuppressWarnings("hiding") String id) {
    this.id = id;
    return this;
  }

  /**
   * Starts tracing.
   *
   * @return the same predicate
   */
  public Predicate<X> stopTracing() {
    this.id = null;
    return this;
  }

  private Iterator<X> filteringIterator(final Iterator<? extends X> i) {
    return new Iterator<X>() {
      X current = null;

      private void find() {
        assert i != null;
        while (current == null && i.hasNext()) {
          X x = i.next();
          if (eval(x)) {
            current = x;
          }
        }
      }

      public boolean hasNext() {
        find();
        trace("filter.hasNext(): ", current != null);
        return current != null;
      }

      public X next() {
        find();
        X result = current;
        current = null;
        trace("filter.next()->", result);
        if (result != null) {
          return result;
        }
        throw new NoSuchElementException();
      }

      public void remove() {
        i.remove();
      }
    };
  }

  /**
   * Filters all the elements of a given iterable of candidates that satisfy the predicate.
   *
   * @param candidates the values at which the predicate is evaluated
   * @return a new (virtual) Iterable of those elements that satisfy the predicate
   */
  public final Iterator<X> filter(final Iterator<? extends X> candidates) {
    assert candidates != null;
    return filteringIterator(candidates);
  }

  /**
   * Filters all the elements of a given iterable of candidates that satisfy the predicate.
   *
   * @param candidates the values at which the predicate is evaluated
   * @return a new (virtual) Iterable of those elements that satisfy the predicate
   */
  public final Iterable<X> filter(final Iterable<? extends X> candidates) {
    assert candidates != null;
    trace("called filter(", candidates, ")");
    return new Iterable<X>() {

      public Iterator<X> iterator() {
        return filter(candidates.iterator());
      }
    };
  }

  /**
   * Filters all the elements of a given set of candidates that satisfy the predicate.
   *
   * @param candidates the values at which the predicate is evaluated
   * @return a new (virtual) set of those elements that satisfy the predicate
   */
  public final Set<X> filter(final Set<? extends X> candidates) {
    assert candidates != null;

    trace("called filter(", candidates, ")");
    return new AbstractSet<X>() {

      @Override
      public Iterator<X> iterator() {
        return filter(candidates.iterator());
      }

      /**
       * This is an expensive method; use it with care (if you ever need to use it at all).
       * @return evaluated size of the resultset
       */
      @Override
      public int size() {
        return Base.countEntries(this);
      }
    };
  }

  /**
   * Finds all the elements of a given iterable of candidates that satisfy the predicate.
   * The candidates are supposed to be unique.
   * The result is a virtual set; nothing is calculated unless elements are retrieved
   * or unless the client code tries to evaluate the size. Since the result is a virtual set,
   * the underlying iterable may change any time, giving different results.
   *
   * @param candidates the values at which the predicate is evaluated
   * @return a new (virtual) set of s which elements satisfy the predicate
   */// !!! Why set? Weird... try to return an iterable
  public final Set<X> find(final Iterable<? extends X> candidates) {
    return Sets.Set(filter(candidates));
  }

  /**
   * Universal quantifier. Checks whether all elements of a given set satisfy the predicate.
   *
   * @param candidates the set at which the predicate is being applied.
   * @return true if all elements of the given set satisfy the predicate; false otherwise
   */
  public final boolean forall(Iterable<X> candidates) {
    trace("called forall(", candidates, ")");
    for (X x : candidates) {
      if (!eval(x)) {
        trace("forall()->false (failed on ", x, ")");
        return false;
      }
    }
    trace("forall()->true");
    return true;
  }

  /**
   * Finds an element in the given set that satisfies the predicate.
   * If nothing is found, returns null.
   *
   * @param candidates all possible candidates
   * @return an arbitrary element of set that satisfies the predicate, or null if none found
   */
  public final X findOne(Iterable<? extends X> candidates) {
    return Base.oneOf(find(candidates));
  }

  /**
   * Checks if there exists an element of a set satisfying the predicate.
   *
   * @param candidates the candidates to check
   * @return true if one of the suggested candidates satisfies the predicate
   */
  public final boolean exists(Iterable<X> candidates) {
    return !find(candidates).isEmpty();
  }

  /**
   * Checks if there is exactly one element among given candidates that satisfies the predicate.
   *
   * @param candidates the candidates to check
   * @return true if there is exactly one such element
   */
  public final boolean existsUnique(Iterable<X> candidates) {
    return find(candidates).size() == 1;
  }

  /**
   * A predicate that checks set membership, given a set
   *
   * @param <X> set element type
   */
  protected static class MembershipPredicate<X> extends Predicate<X> {
    /**
     * the set which membership is being tested.
     */
    Set<X> xs;

    /**
     * Constructor. Builds a membership predicate for the set provided.
     * @param xs the set
     */
    MembershipPredicate(Set<X> xs) {
      this.xs = xs;
    }

    @Override
    public boolean eval(X x) {
      return xs.contains(x);
    }
  }

  /**
   * Creates a predicate that checks whether a value belongs to a set.
   * @param <X> element type
   *
   * @param xs the set
   * @return the predicate
   */
  public static <X> Predicate<X> forSet(final Set<X> xs) {
    return new Predicate<X>() {
      @Override
      public boolean eval(X x) {
        return xs.contains(x);
      }
    };
  }
}
