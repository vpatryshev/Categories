package java.math.cat;

import static java.math.cat.Base.equal;

import java.util.Map;

/**
 * Abstract pair class.
 * Implementations should implement two methods, and may be lazy or regular.
 * But in any case this is an unmodifiable pair.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 * @param <X> first component type
 * @param <Y> second component type
 */
public abstract class Pair<X, Y> implements Map.Entry<X, Y> {

  /**
   * @return first element of the pair.
   */
  public abstract X x();

  /**
   * @return second element of the pair.
   */
  public abstract Y y();

  /**
   * @return first element of the pair.
   */
  public X getKey() {
    return x();
  }

  /**
   * @return second element of the pair.
   */
  public Y getValue() {
    return y();
  }

  /**
   * Setter method; not implemented.
   *
   * @inheritDoc
   */
  public Y setValue(Y value) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof Pair && equal(((Pair<?, ?>) o).x(), x()) && equal(((Pair<?, ?>) o).y(), y());
  }

  /**
   * Hashcode for a pair. HAS TO use the same algorithm as Math.Entry implementations,
   * or else oops.
   *
   * @return the hashcode...
   */
  @Override
  public int hashCode() {
    return Base.hashCode(x()) ^ Base.hashCode(y());
  }

  @Override
  public String toString() {
    return "(" + x() + "," + y() + ")";
  }

  private static <X,Y> Pair<X,Y> pair(final X x0, final Y y0) {
    return new Pair<X, Y>() {
      public X x() { return x0; }
      public Y y() { return y0; }
    };
  }

  public static <X,Y> Pair<X,Y> of(X x, Y y) {
    return pair(x, y);
  }

  public static <X> Pair<X,X> from(X[] source) {
    assert source.length == 2 : "Pair is built on a two-element array; got " + source.length;
    return pair(source[0], source[1]);
  }

}

