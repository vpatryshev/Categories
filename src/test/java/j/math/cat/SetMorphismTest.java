package j.math.cat;

import static j.math.cat.SetMorphism.*;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Unittest for SetMorphism class
 */
@SuppressWarnings({"rawtypes","unchecked"})
public class SetMorphismTest extends TestCase {
  Set<Integer> ints = new HashSet<Integer>(Arrays.asList(1, 2, 3, 5, 8, 13));
  Set<String> strings = new HashSet<String>(Arrays.asList("even", "odd", "totally crazy"));
  SetMorphism<Integer, Set<Integer>, String, Set<String>> m =
    new SetMorphism<Integer, Set<Integer>, String, Set<String>>(ints, strings) {
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