package j.math.cat;

import static j.math.cat.BasePair.Pair;
import static j.math.cat.Sets.Set;
import junit.framework.TestCase;

/**
 * Unittest for BinaryRelation class
 * 
 *
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 *
 */
public class BinaryRelationTest extends TestCase {

  @SuppressWarnings("unchecked")
  public void testForPairs() {
    BinaryRelation<String, Integer> r =
      BinaryRelation.forPairs(Sets.Set(Pair("one", 1), Pair("two", 2)));
    assertTrue(r.eval("one", 1));
    assertTrue(r.eval("two", 2));
    assertFalse(r.eval("two", 1));
    assertFalse(r.eval("one", 2));
  }

}
