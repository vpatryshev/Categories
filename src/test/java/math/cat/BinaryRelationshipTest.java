package math.cat;

import static java.math.cat.BasePair.Pair;
import static java.math.cat.Sets.Set;
import junit.framework.TestCase;

/**
 * Unittest for BinaryRelationship class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class BinaryRelationshipTest extends TestCase {

  @SuppressWarnings("unchecked")
  public void testForPairs() {
    java.math.cat.BinaryRelationship<String, Integer> r =
      java.math.cat.BinaryRelationship.forPairs(Set(Pair("one", 1), Pair("two", 2)));
    assertTrue(r.eval("one", 1));
    assertTrue(r.eval("two", 2));
    assertFalse(r.eval("two", 1));
    assertFalse(r.eval("one", 2));
  }

}
