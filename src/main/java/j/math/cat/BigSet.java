package j.math.cat;

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

/**
 * An implementation of Set that cannot enumerate its elements.
 * AC is also optional here.
 * @param <T> element type
 *
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 */
public abstract class BigSet<T> implements Set<T> {
  @Override
  public int size() {
    return Integer.MAX_VALUE;
  }

  public String whoami() { return "A BIG SET"; }

  @Override public String toString() { return whoami(); }

  @Override
  public boolean isEmpty() {
    throw new UnsupportedOperationException("Emptiness unknown");
  }

  @Override
  public Iterator<T> iterator() {
    throw new UnsupportedOperationException("This set is not enumerable: " + this);
  }

  @Override
  public Object[] toArray() {
    throw new UnsupportedOperationException("This set is not enumerable");
  }

  @Override
  public <E> E[] toArray(E[] a) {
    throw new UnsupportedOperationException("This set is not enumerable");
  }

  @Override
  public boolean add(T setMorphism) {
    throw new UnsupportedOperationException("This set is immutable");
  }

  @Override
  public boolean remove(Object o) {
    throw new UnsupportedOperationException("This set is immutable");
  }

  @Override
  public boolean containsAll(Collection<?> c) {
    for (Object a : c) {
      if (!contains(a)) {
        return false;
      }
    }
    return true;
  }

  @Override
  public boolean addAll(Collection<? extends T> c) {
    throw new UnsupportedOperationException("This set is immutable");
  }

  @Override
  public boolean retainAll(Collection<?> c) {
    throw new UnsupportedOperationException("This set is immutable");
  }

  @Override
  public boolean removeAll(Collection<?> c) {
    throw new UnsupportedOperationException("This set is immutable");
  }

  @Override
  public void clear() {
    throw new UnsupportedOperationException("This set is immutable");
  }

  /**
   * A big set of all finite sets in Java. This set is infinite, of course.
   */
  @SuppressWarnings({"rawtypes","unchecked"})
  public final static BigSet<Set> FINITE_SETS = new BigSet<Set>() {

    @Override
    public boolean contains(Object o) {
      return o instanceof Set && ((Set) o).size() < Integer.MAX_VALUE;
    }

    public String whoami() { return "SetF, the class of all finite sets"; }
  };
}
