package math.cat;

import static math.cat.Base.*;
import static math.cat.Pair.*;

import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;

/**
 * Sample Implementation of partially ordered set.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public abstract class PoSet<T> extends AbstractSet<T> {
  private final Set<T> elements;

  /*
   * Java technicalities: have to override these methods.
   */
  @Override public Iterator<T> iterator() {  return elements.iterator(); }
  @Override public int size() { return elements.size(); }
  @Override public int hashCode() { return elements.hashCode(); }
  @Override public boolean equals(Object o) { return o instanceof PoSet && ((PoSet)o).equals(this); }
  
  /**
   * Defines partial order.
   * @param x first compared element
   * @param y second compared element
   * @return true if x is before y in this partial order
   */
  public abstract boolean _le_(T x, T y);

  /**
   * Basic constructor. You need to define a comparator method to build an instance.
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
    for(T x : elements) {
      assert _le_(x, x): " reflexivity broken at " + x;

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
    for (T a : elements) for (T b : elements) {
      isEqual = isEqual && (_le_(a, b) == other._le_(a, b));
    }
    return isEqual;
  }

  /**
   * Builds a poset based on pairs of elements that define the partial order.
   * @param elements elements of this poset
   * @param pairs pairs of comparable elements
   * @return a new poset built on the data provided
   */
  public static <X> PoSet<X> PoSet(Set<X> elements, final Set<Pair<X,X>> pairs) {
    return new PoSet<X>(elements) {
      public boolean _le_(X a, X b) { return pairs.contains(Pair(a, b)); }
    };
  }

  /**
   * Builds a new poset out of this one, with the inverted order.
   * @return a new poset with the order that is opposite to the original.
   */
  public PoSet<T> op() {
    final PoSet<T> source = this;
    return new PoSet<T>(elements) {
      public boolean _le_(T x, T y) {
        return source._le_(y, x);
      }
    };
  }

  public String toString() {
    StringBuffer out = new StringBuffer();

    for (T x : elements) {
      for(T y : elements) {
        if (_le_(x, y)) {
          if (out.length() > 0) out.append(", ");
          out.append(x).append("<=").append(y);
        }
      }
    }
    return elements.toString() + "{" + out + "}";
  }

  /**
   * Parses a poset from a kind of string that is produced by toString()
   * @param source source string
   * @return a parsed poset
   */
  public static PoSet<String> PoSet(String source) {
    int splitAt = source.indexOf("]{"); // separates elements from comparisions
    Set<Pair<String, String>> pairs = new HashSet<Pair<String, String>>();
    for (String comparison : source.substring(splitAt + 2, source.length() - 1).split(",\\s*")) {
      String[] pair = comparison.split("<=");
      if (pair.length == 2) pairs.add(Pair(pair[0].trim(), pair[1].trim()));
    }

    return PoSet(Base.parseSet(source.substring(0, splitAt + 1)), pairs);
  }

  /**
   * Builds a linear poset consisting of a range of integers, with their natural order.
   *
   * @param from the first integer in the range
   * @param to the last intger in the range (included)
   * @param step distance between two consecutive elements
   * @return a new poset 
   */
  public static PoSet<Integer> range(int from, int to, int step) {
    Set<Integer> elements = new HashSet<Integer>();

    for (int i = from; i <= to; i += step) {
      elements.add(i);
      if (step == 0) break;
    }

    return new PoSet<Integer>(elements) {
      public boolean _le_(Integer x, Integer y) { return x <= y; }
    };
  }

  public static void main(String[] args) {
    PoSet<String> ex1 = new PoSet<String>(Set("abc", "def", "ab", "defgh")) {
      public boolean _le_(String a, String  b) {
        return b.indexOf(a) >= 0;
      }
    };
    PoSet<String> ex2 = new PoSet<String>(Set("abc", "def", "ab", "defgh")) {
      public boolean _le_(String a, String  b) {
        return b.indexOf(a) == 0;
      }
    };

    System.out.println("Posets " + ex1 + "\nand " + ex2 + "\nare " +
        (ex1.equals(ex2) ? "" : "not ") + "equal");
  }
}
