package j.math.cat;

/**
 * Delegating pair class. An implementation of Pair that delegates
 * everything to another pair.
 * 
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * 
 * @param <X> first component type
 * @param <Y> second component type
 */
public class DelegatingPair<X, Y> extends Pair<X, Y> {
  
  /**
   * Delegate pair. 
   */
  Pair<X, Y> delegate;

  /**
   * @inheritDoc
   */
  @Override
  public X x() {
    return delegate.x();
  }

  /**
   * @inheritDoc
   */
  @Override
  public Y y() {
    return delegate.y();
  }

  /**
   * Constructor.
   *
   * @param delegate the delegate pair
   */
  public DelegatingPair(Pair<X, Y> delegate) {
    this.delegate = delegate;
  }
}