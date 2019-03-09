package j.math.cat;

/**
 * Base pair class. An implementation of Pair where both x and y
 * are given explicitly.
 * @param <X> first component type
 * @param <Y> second component type
 */
public class BasePair<X, Y> extends Pair<X, Y> {
  private final X x;
  private final Y y;

  /**
   * @inheritDoc
   */
  @Override
  public X x() {
    return x;
  }

  /**
   * @inheritDoc
   */
  @Override
  public Y y() {
    return y;
  }

  /**
   * Constructor.
   *
   * @param x first element
   * @param y second element
   */
  public BasePair(X x, Y y) {
    this.x = x;
    this.y = y;
  }

  /**
   * Factory method.
   * 
   * @param <X> first component type
   * @param <Y> second component type
   * @param x first element
   * @param y second element
   * @return a new pair (x, y)
   */
  public static <X, Y> Pair<X, Y> Pair(X x, Y y) {
    return new BasePair<X, Y>(x, y);
  }

  /**
   * Factory method. Builds a pair from a two-element array.
   * 
   * @param <X> component type (same for both components)
   * @param source a two-element array, [x, y].
   * @return a new pair, (x, y)
   */
  public static <X> Pair<X, X> Pair(X[] source) {
    assert source.length == 2 :
        "BasePair should be built on a two-element array; got " + source.length;
    return Pair(source[0], source[1]);
  }

  /**
   * Builds a function that builds pairs attaching x as the first element.
   * 
   * @param <X> first component type 
   * @param <Y> second component type
   * @param x the element to attach.
   * @return a function y -> (x, y)
   */
  public static <X, Y> Functions.Injection<Y, Pair<X, Y>> withLeft(final X x) {
    return new Functions.Injection<Y, Pair<X, Y>>() {

      @Override
      public Pair<X, Y> apply(Y y) {
        return Pair(x, y);
      }
    };
  }

  /**
   * Builds a function that builds pairs attaching y as the second element.
   * 
   * @param <X> first component type 
   * @param <Y> second component type
   * @param y the element to attach.
   * @return a function x -> (x, y)
   */
  public static <X, Y> Functions.Injection<X, Pair<X, Y>> withRight(final Y y) {
    return new Functions.Injection<X, Pair<X, Y>>() {

      @Override
      public Pair<X, Y> apply(X x) {
        return Pair(x, y);
      }
    };
  }
}

