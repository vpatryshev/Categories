package math.cat;

import static math.cat.Base.*;

import java.util.*;
import junit.framework.TestCase;

/**
 * Tests for math.cat.Base class
 */
public class BaseTest extends TestCase {
  public void testParse() {
    assertEquals(new HashSet<String>(Arrays.asList("abc", "def")), Base.parseSet("[abc, def]"));
  }

  public void testParse_empty() {
    assertEquals(new HashSet<String>(), Base.parseSet("[]"));
  }

  public void testParse_toStringAndBack() {
    Set<String> testSet = new HashSet<String>(Arrays.asList("abc", "def"));
    assertEquals(testSet, Base.parseSet(testSet.toString()));
  }

  public void testParse_evilWithSpaces() {
    Set<String> testSet = new HashSet<String>(Arrays.asList("a b c", "d e f"));
    assertEquals(testSet, Base.parseSet(testSet.toString()));
  }

  public void testId_plain() {
    Map<Integer, Integer> expected = new HashMap<Integer, Integer>() {{
      put(0, 0); put(1, 1); put(2, 2);
    }};

    assertEquals(expected, id(Set(0, 1, 2)));
  }
  
  public void testId_empty() {
    assertEquals(Collections.EMPTY_MAP, id(Collections.EMPTY_SET));
  }
}
