package math.cat;

import static math.cat.Pair.*;

import junit.framework.TestCase;

/**
 * Tests for math.cat.Pair class
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
}
