package j.math.cat;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Tests for Predicate class
 */
public class PredicateTest extends TestCase {
  private static Predicate<Integer> divisibleBy(final int d) {
    return new Predicate<Integer>() {
      @Override
      public boolean eval(Integer n) {
        return n % d == 0;
      }
    };
  }

  public void testFind() {
    Iterable<Integer> source = Sets.Set(2, 3, 6, 7, 9, 10);
    TestCase.assertEquals(Sets.Set(3, 6, 9), divisibleBy(3).find(source));
    TestCase.assertEquals(Sets.Set(3, 6, 9), divisibleBy(3).find(Sets.Set(3, 6, 7, 9)));
  }

  public void testFindOne_plain() {
    TestCase.assertTrue(Sets.Set(3, 6, 9).contains(divisibleBy(3).findOne(Sets.Set(2, 3, 6, 7, 9, 10))));
  }

  public void testFindOne_none() {
    assertEquals(null, divisibleBy(8).findOne(Sets.Set(2, 3, 6, 7, 9, 10)));
  }

  public void testForall_positive() {
    assertTrue(divisibleBy(3).forall(Sets.Set(3, 6, 9)));
  }

  public void testForall_negative() {
    assertFalse(divisibleBy(3).forall(Sets.Set(3, 6, 9, 11)));
  }

  public void testForall_empty() {
    assertTrue(divisibleBy(3).forall(new HashSet<Integer>()));
  }

  public void testExists_positive() {
    assertTrue(divisibleBy(3).exists(Sets.Set(3, 2, 9)));
  }

  public void testExists_negative() {
    assertFalse(divisibleBy(3).exists(Sets.Set(2, 7, 11)));
  }

  public void testExists_empty() {
    assertFalse(divisibleBy(3).exists(new HashSet<Integer>()));
  }

  public void testExistsUnique_positive() {
    assertTrue(divisibleBy(3).existsUnique(Sets.Set(3, 2, 7)));
  }

  public void testExistsUnique_tooMany() {
    assertFalse(divisibleBy(3).existsUnique(Sets.Set(3, 2, 9)));
  }

  public void testExistsUnique_noneFound() {
    assertFalse(divisibleBy(3).exists(Sets.Set(2, 7, 11)));
  }

  public void testExistsUnique_empty() {
    assertFalse(divisibleBy(3).exists(new ArrayList<Integer>()));
  }

  public void testForSet() {
    Set<Integer> testSet = Sets.Set(3, 17, 90);
    Predicate<Integer> p = Predicate.forSet(testSet);
    for (Integer i : testSet) {
      assertTrue(p.eval(i));
    }
    assertFalse(p.eval(0));
  }
}