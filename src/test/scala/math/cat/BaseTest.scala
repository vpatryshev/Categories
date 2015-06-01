package math.cat

import math.cat.Base._

import org.specs2._

/**
 * Tests for math.cat.Base class
 */
public class BaseTest extends Specification {
  def is = s2"""

  This is a specification for the 'Hello world' string

  The 'Hello world' string should
  contain 11 characters                             $e1
  start with 'Hello'                                $e2
  end with 'world'                                  $e3
  """
  def e1 = "Hello world" must haveSize(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")

}/*
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
  }*/
}
