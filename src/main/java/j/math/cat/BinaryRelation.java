package j.math.cat;

import static j.math.cat.BasePair.Pair;

import java.util.Set;

/**
 * Java representation of binary relationship, that is, a two-parameter predicate.
 * A predicate is a logical-valued function. Here in Java we use boolean for logical.
 * 
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * Special thanks to Eugene Kirpichov (antilamer.livejournal.com) for inspirational ideas.
 * <p/>
 * <p/>
 * A bunch of helpful methods that apply predicates to sets, producing virtual derivative sets will be added later.
 * @param <X> first argument type
 * @param <Y> second argument type
 */
public abstract class BinaryRelation<X, Y> extends Predicate<Pair<X, Y>> {
  /**
   * Evaluates the predicate. You have to define this method in your specific predicate.
   * Calls <code>eval(Pair(x, y))</code> actually.
   *
   * @param x the first component of the value at which the predicate is evaluated
   * @param y the second component of the value at which the predicate is evaluated
   * @return predicate value
   */
  public boolean eval(X x, Y y) {
    return eval(BasePair.Pair(x, y));
  }

  /**
   * Creates a relationship that checks against a set of given pairs.
   *
   * @param pairs the set
   * @return the predicate
   * @param <X> first argument type
   * @param <Y> second argument type
   */
  public static <X, Y> BinaryRelation<X, Y> forPairs(final Set<Pair<X, Y>> pairs) {
    return new BinaryRelation<X, Y>() {
      @Override
      public boolean eval(Pair<X, Y> x) {
        return pairs.contains(x);
      }
    };
  }
}