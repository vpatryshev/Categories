package java.math.cat;

import static java.math.cat.SetMorphism.compose;
import static java.math.cat.SetMorphism.unit;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Unittest for SetMorphism class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class SetMorphismTest extends TestCase {
  Set<Integer> ints = new HashSet<Integer>(Arrays.asList(1, 2, 3, 5, 8, 13));
  Set<String> strings = new HashSet<String>(Arrays.asList("even", "odd", "totally crazy"));
  java.math.cat.SetMorphism<Integer, Set<Integer>, String, Set<String>> m =
    new java.math.cat.SetMorphism<Integer, Set<Integer>, String, Set<String>>(ints, strings) {
        @Override
        public String apply(Integer integer) {
          return integer % 2 == 0 ? "even" : "odd";
        }
      };

  public void testConstructor() {
    assertEquals("even", m.apply(8));
    assertEquals("odd", m.apply(13));
    try {
      m.apply(4);
      fail("4 does not belong to domain; should have thrown an error");
    } catch (Throwable e) {
      // as expected
    }
  }

  public void testUnitComposition() {
    assertEquals(m, compose(m, unit(strings)));
    assertEquals(m, compose(unit(ints), m));
  }
}