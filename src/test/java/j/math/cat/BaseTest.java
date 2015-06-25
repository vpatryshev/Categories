package j.math.cat;

import static j.math.cat.Base.flatten;
import static j.math.cat.Base.oneOf;
import static j.math.cat.Base.split;
import static j.math.cat.Sets.Set;
import static j.math.cat.Sets.numbers;

import j.math.cat.Functions.Injection;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Tests for Base class
 * 
 * @author Vlad Patryshev
 * All source code is stored  * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>

 * 
 */
public class BaseTest extends TestCase {

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_plain() {
    Set<String> actual = Base.disjointUnion(Sets.Set("ab", "ac", "ad", "bd", "cd"), Sets.Set("a", "b", "c", "d"));
    Set<String> expected = Sets.Set("a", "b", "c", "d", "ab", "ac", "ad", "bd", "cd");
    int count = 0;
    for (@SuppressWarnings("unused") String s : actual) {
      count++;
    }
    assertEquals(expected.size(), count);
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_withEmpty() {
    Set<String> actual = Base.disjointUnion(new HashSet<String>(), Sets.Set("ab", "ac", "ad", "bd", "cd"), new HashSet(), Sets.Set("a", "b", "c", "d"), new HashSet());
    Set<String> expected = Sets.Set("a", "b", "c", "d", "ab", "ac", "ad", "bd", "cd");
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_empty() {
    TestCase.assertEquals(Collections.EMPTY_SET, Base.disjointUnion());
  }

  private static <A, B> void assertRightInverse(Map<A, B> f, Map<B, A> g) {
    for (A a : f.keySet()) {
      assertEquals(a, g.get(f.get(a)));
    }
  }

  @SuppressWarnings("serial")
  public void testInverse_plain() {
    Map<String, Integer> m = new HashMap<String, Integer>() {
      {
        put("I", 1);
        put("II", 2);
        put("III", 3);
        put("IV", 4);
      }
    };

    Map<Integer, String> actual = Base.inverse(m);
    assertRightInverse(m, actual);
    assertRightInverse(actual, m);
  }

  @SuppressWarnings("serial")
  public void testInverse_empty() {
    Map<String, Integer> m = new HashMap<String, Integer>() {
    };

    Map<Integer, String> actual = Base.inverse(m);
    assertRightInverse(m, actual);
    assertRightInverse(actual, m);
  }

  @SuppressWarnings("serial")
  public void testInverse_negative() {
    Map<String, Integer> m = new HashMap<String, Integer>() {
      {
        put("I", 1);
        put("II", 2);
        put("III", 3);
        put("iii", 3);
      }
    };

    try {
      Base.inverse(m);
      fail("cannot invert this map: " + m);
    } catch (Throwable t) {
      // as expected
    }
  }

  public void testOneOf_forIterableEmpty() {
    TestCase.assertEquals(null, Base.oneOf(Arrays.asList()));
  }

  public void testOneOf_forIterable() {
    TestCase.assertTrue(Sets.Set("a", "b").contains(Base.oneOf(Arrays.asList("a", "b"))));
  }

  public void testOneOf_forVarargEmpty() {
    TestCase.assertEquals(null, Base.oneOf());
  }

  public void testOneOf_forVararg() {
    TestCase.assertTrue(Sets.Set("a", "b").contains(Base.oneOf("a", "b")));
  }

  private Iterable<Iterable<Integer>> triangle(final int n) {
    return new Iterable<Iterable<Integer>>() {
      public Iterator<Iterable<Integer>> iterator() {
        return new Iterator<Iterable<Integer>>() {
          int i = n;

          public boolean hasNext() {
            return i-- > 0;
          }

          public Iterable<Integer> next() {
            return Sets.numbers(i, -1, -1);
          }

          public void remove() {
          }
        };
      }
    };
  }

  public void testFlatten() {
    List<Integer> storage = new ArrayList<Integer>();
    for (int i : Base.flatten(triangle(5))) {
      storage.add(i);
    }
    assertEquals(Arrays.asList(4, 3, 2, 1, 0, 3, 2, 1, 0, 2, 1, 0, 1, 0, 0), storage);
  }

  public void testApply_plain() {
    Set<String> actual = new HashSet<String>();
    for (String s : new Injection<Integer, String>() {
      @Override
      public String apply(Integer integer) {
        return "" + integer;
      }
    }.map(Sets.numbers(3))) {
      actual.add(s);
    }

    TestCase.assertEquals(Sets.Set("0", "1", "2"), actual);
  }

  public void testFlatten_withApply() {
    List<Integer> storage = new ArrayList<Integer>();
    for (int i : flatten(new Injection<Integer, Iterable<Integer>>() {
      @Override
      public Iterable<Integer> apply(Integer n) {
        return Sets.numbers(n + 1);
      }
    }.map(Sets.numbers(5)))) {
      storage.add(i);
    }
    assertEquals(Arrays.asList(0, 0, 1, 0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4), storage);
  }

  public void testSplit_plain() {
    Iterable<String> i = Arrays.asList("a", "b", "c");
    Pair<String, Iterable<String>> p1 = Base.split(i);
    assertEquals("a", p1.x());
    Pair<String, Iterable<String>> p2 = Base.split(p1.y());
    assertEquals("b", p2.x());
    Pair<String, Iterable<String>> p3 = Base.split(p2.y());
    assertEquals("c", p3.x());
    assertFalse(p3.y().iterator().hasNext());
  }

  @SuppressWarnings("unchecked")
  public void testSplit_deep() {
    Iterable<Iterable<Integer>> i = Arrays.<Iterable<Integer>>asList(Arrays.asList(1, 2, 3), Arrays.asList(4, 5));
    Pair<Iterable<Integer>, Iterable<Iterable<Integer>>> p1 = Base.split(i);
    TestCase.assertEquals(Sets.Set(1, 2, 3), Sets.Set(p1.x()));
    TestCase.assertEquals(Sets.Set(4, 5), Sets.Set(p1.y().iterator().next()));
  }
}
