package math.cat;

import static math.cat.Maybe.*;

import junit.framework.TestCase;
import math.cat.Maybe;

/**
 * Tests for math.cat.Pair class
 */
public class MaybeTest extends TestCase {
  public void testConstructor_plain() {
    assertEquals("hello world", some("hello world").value());
    assertEquals(3.14159265358, some(3.14159265358).value());
    assertEquals(NONE, some(null));
  }

  public void testHasValue_plain() {
    assertTrue(some("hello world").hasValue());
    assertTrue(some(3.14159265358).hasValue());
    assertFalse(NONE.hasValue());
  }

  public void testValue_shouldThrowException() {
    try {
      some(null).value();
      fail("Must have thrown exception");
    } catch (Exception e) {
      // as expected
    }
    try {
      NONE.value();
      fail("Must have thrown exception");
    } catch (Exception e) {
      // as expected
    }
  }

  public void testEquals() {
    assertTrue(NONE.equals(some(null)));
    assertFalse(NONE.equals(some(false)));
    assertFalse(some(true).equals(NONE));
    int n1 = (int)((System.currentTimeMillis() * 27 + 92) % 3);
    assertTrue(some("ab2").equals(some("baobab".substring(4) + n1)));
    long n2 = (System.currentTimeMillis() * 10000L + 1001590L) % 10000L;
    assertTrue(some(1590L).equals(some(n2)));
  }

  public void testIdempotence() {
    Maybe<String> x = some("XXX");
    assertEquals("XXX", some(some(x)).value());
  }

  public void testToString() {
    assertEquals("NONE", NONE.toString());
    assertEquals("lorem ipsum", some("lorem ipsum").toString());
  }
  
  public void testHashCode() {
    assertEquals(0, NONE.hashCode());
    assertEquals("e lorebus ipsum".hashCode(), some("e lorebus ipsum").hashCode());
  }
}