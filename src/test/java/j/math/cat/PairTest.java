package j.math.cat;

import j.math.cat.Functions.Function;
import junit.framework.TestCase;

/**
 * Tests for Pair class.
 * 
 *
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * 
 * <p/>
 * Note. Please set "-ea" to your vm parameters, to enable assertions.
 */
public class PairTest extends TestCase {
  public void testEquals_positive() {
    String s = new StringBuilder("a").append(1 == 2 ? "cb" : "bc").toString();
    TestCase.assertEquals(BasePair.Pair("abc", "def"), BasePair.Pair(s, "def"));
  }

  public void testEquals_negativeX() {
    String s = new StringBuilder("a").append(1 == 1 ? "cb" : "bc").toString();
    TestCase.assertFalse(BasePair.Pair("abc", "def").equals(BasePair.Pair(s, "def")));
  }

  public void testEquals_negativeY() {
    String s = new StringBuilder("a").append(1 == 2 ? "cb" : "bc").toString();
    TestCase.assertFalse(BasePair.Pair("abc", "def").equals(BasePair.Pair(s, "duh...")));
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

    Pair<String, Integer> p0 = LazyPair.LazyPair("zero", f);
    Pair<String, Integer> p1 = LazyPair.LazyPair("one", f);
    shouldBeCalled[0] = true;
    assertEquals(Integer.valueOf(0), p0.y());
    assertEquals(Integer.valueOf(1), p1.y());
    shouldBeCalled[0] = false;
    assertEquals(Integer.valueOf(0), p0.y());
    assertEquals(Integer.valueOf(1), p1.y());
  }
}
