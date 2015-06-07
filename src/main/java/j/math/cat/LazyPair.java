package j.math.cat;

import j.math.cat.Functions.Function;

/**
 * Lazy pair class.
 * Given a Function<X, Y> f, and an X x, lazily stores a pair (x, f(x)).
 * Laziness means that f(x) is not calculated until requested.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 * @param <X> first component type
 * @param <Y> second component type
 */
public class LazyPair<X, Y> extends Pair<X, Y> {
  private final X x;
  private final Function<X, Y> f;
  private Y y;
  private boolean haveY = false;

  private LazyPair(X x, Function<X, Y> f) {
    this.x = x;
    this.f = f;
  }

  /**
   * Factory for lazy pairs.
   * 
   * @param <X> first component type 
   * @param <Y> second component type
   * @param x the argument
   * @param f the function to apply
   * @return a new lazy pair
   */
  public static <X, Y> LazyPair<X, Y> LazyPair(X x, Function<X, Y> f) {
    return new LazyPair<X, Y>(x, f);
  }

  @Override
  public X x() {
    return x;
  }

  @Override
  public Y y() {
    if (!haveY) {
      y = f.apply(x);
      haveY = true;
    }
    return y;
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof LazyPair ?
        Base.equal(x(), ((LazyPair<?, ?>) o).x()) : o.equals(this);
  }

  @Override
  public int hashCode() {
    return Base.hashCode(x);
  }
}
