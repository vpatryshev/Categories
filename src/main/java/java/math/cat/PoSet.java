package java.math.cat;

import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Set;

/**
 * Sample Implementation of partially ordered set.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public abstract class PoSet<T> extends AbstractSet<T> {
  private final Set<T> elements;

  /*
   * Java technicalities: have to override these methods.
   */
  @Override
  public Iterator<T> iterator() {
    return elements.iterator();
  }

  @Override
  public int size() {
    return elements.size();
  }

  @Override
  public int hashCode() {
    return elements.hashCode();
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof PoSet && ((PoSet) o).equals(this);
  }

  /**
   * Defines partial order.
   *
   * @param x first compared element
   * @param y second compared element
   * @return true if x is before y in this partial order
   */
  public abstract boolean _le_(T x, T y);

  /**
   * Basic constructor. You need to define a comparator method to build an instance.
   *
   * @param elements elements of this poset.
   */
  public PoSet(Set<T> elements) {
    this.elements = elements;
    validate();
  }

  /**
   * Validates the axioms of this partially ordered set
   */
  private void validate() {
    for (T x : elements) {
      assert _le_(x, x) : " reflexivity broken at " + x;

      for (T y : elements) {
        if (_le_(x, y)) {
          if (_le_(y, x)) assert x.equals(y) : "antisymmetry broken at " + x + ", " + y;

          for (T z : elements) {
            if (_le_(y, z)) assert _le_(x, z) : "transitivity broken at " + x + ", " + y + ", " + z;
          }
        }
      }
    }
  }

  /**
   * Two posets are equal if they have the same elements and partial order is the same.
   *
   * @param other other poset to compare
   * @return true if these two posets are equal
   */
  private boolean equals(PoSet<T> other) {
    boolean isEqual = elements.equals(other.elements);
    for (T a : elements)
      for (T b : elements) {
        isEqual = isEqual && (_le_(a, b) == other._le_(a, b));
      }
    return isEqual;
  }
}
