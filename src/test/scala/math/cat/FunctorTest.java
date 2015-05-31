package math.cat;

import static math.cat.Functor.*;
import static math.cat.Pair.*;
import junit.framework.TestCase;

import java.util.HashMap;

/**
 * Unittest for Functor class.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public class FunctorTest extends TestCase {

  public void testFunctor_missingObjectMapppings() {
    try {
      Functor<Integer, Pair<Integer, Integer>, Integer, Pair<Integer, Integer>> f =
          Functor(
              Category._4_,
              Category._4_,
              new HashMap<Integer, Integer>() {{
                put(0, 1);
              }},
              new HashMap<Pair<Integer, Integer>, Pair<Integer, Integer>>()
          );
      fail("This constructor should have failed");
    } catch(Throwable e) {
      // as expected
    }
  }

  public void testFunctor_missingArrowMapppings() {
    try {
      Functor<Integer, Pair<Integer, Integer>, Integer, Pair<Integer, Integer>> f =
          Functor(
              Category._4_,
              Category._4_,
              new HashMap<Integer, Integer>() {{
                put(0, 1);
                put(1, 2);
                put(2, 3);
                put(3, 3);
              }},
              new HashMap<Pair<Integer, Integer>, Pair<Integer, Integer>>()
          );
      fail("This constructor should have failed");
    } catch(Throwable e) {
      // as expected
    }
  }

  public void testFunctor_unitBroken() {
    try {
      Functor<Integer, Pair<Integer, Integer>, Integer, Pair<Integer, Integer>> f =
          Functor(
              Category._4_,
              Category._4_,
              new HashMap<Integer, Integer>() {{
                put(0, 1);
                put(1, 2);
                put(2, 3);
                put(3, 3);
              }},
              new HashMap<Pair<Integer, Integer>, Pair<Integer, Integer>>() {{
                put(Pair(0, 0), Pair(1, 1));
                put(Pair(0, 1), Pair(1, 2));
                put(Pair(0, 2), Pair(1, 3));
                put(Pair(0, 3), Pair(1, 3));
                put(Pair(1, 1), Pair(2, 3));
                put(Pair(1, 2), Pair(2, 3));
                put(Pair(1, 3), Pair(2, 3));
                put(Pair(2, 2), Pair(3, 3));
                put(Pair(2, 3), Pair(3, 3));
                put(Pair(3, 3), Pair(3, 3));
              }}
          );
      fail("This constructor should have failed");
    } catch(Throwable e) {
      // as expected
    }
  }
  
  public void testFunctor_plain() {
    try {
      Functor<Integer, Pair<Integer, Integer>, Integer, Pair<Integer, Integer>> f =
          Functor(
              Category._4_,
              Category._4_,
              new HashMap<Integer, Integer>() {{
                put(0, 1);
                put(1, 2);
                put(2, 3);
                put(3, 3);
              }},
              new HashMap<Pair<Integer, Integer>, Pair<Integer, Integer>>() {{
                put(Pair(0, 0), Pair(1, 1));
                put(Pair(0, 1), Pair(1, 2));
                put(Pair(0, 2), Pair(1, 3));
                put(Pair(0, 3), Pair(1, 3));
                put(Pair(1, 1), Pair(2, 2));
                put(Pair(1, 2), Pair(2, 3));
                put(Pair(1, 3), Pair(2, 3));
                put(Pair(2, 2), Pair(3, 3));
                put(Pair(2, 3), Pair(3, 3));
                put(Pair(3, 3), Pair(3, 3));
              }}
          );
      fail("This constructor should have failed");
    } catch(Throwable e) {
      // as expected
    }
  }
}
