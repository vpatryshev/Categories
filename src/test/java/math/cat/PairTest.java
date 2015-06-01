package math.cat;

import static math.cat.BasePair.Pair;
import static math.cat.LazyPair.LazyPair;
import math.cat.Functions.Function;
import junit.framework.TestCase;

/**
 * Tests for math.cat.Pair class.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 * <p/>
 * Note. Please set "-ea" to your vm parameters, to enable assertions.
 */
public class PairTest extends TestCase {
  public void testEquals_positive() {
    String s = new StringBuilder("a").append(1 == 2 ? "cb" : "bc").toString();
    assertEquals(Pair("abc", "def"), Pair(s, "def"));
  }

  public void testEquals_negativeX() {
    String s = new StringBuilder("a").append(1 == 1 ? "cb" : "bc").toString();
    assertFalse(Pair("abc", "def").equals(Pair(s, "def")));
  }

  public void testEquals_negativeY() {
    String s = new StringBuilder("a").append(1 == 2 ? "cb" : "bc").toString();
    assertFalse(Pair("abc", "def").equals(Pair(s, "duh...")));
  }

  public void testLazyPair() {
    final boolean[] shouldBeCalled = {false};
    Function<String, Integer> f = new Function<String, Integer>() {
      int counter = 0;

      @Override
      public Integer apply(String s) {
        if (!shouldBeCalled[0]) {
          fail("should not have been called with " + s);
        }
        return counter++;
      }
    };

    Pair<String, Integer> p0 = LazyPair("zero", f);
    Pair<String, Integer> p1 = LazyPair("one", f);
    shouldBeCalled[0] = true;
    assertEquals(Integer.valueOf(0), p0.y());
    assertEquals(Integer.valueOf(1), p1.y());
    shouldBeCalled[0] = false;
    assertEquals(Integer.valueOf(0), p0.y());
    assertEquals(Integer.valueOf(1), p1.y());
  }
}
