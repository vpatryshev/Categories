package j.math.cat;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Tests for Base class
 * 
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * 
 */
public class FunctionsTest extends TestCase {

  @SuppressWarnings("serial")
  public void testId_plain() {
    Map<Integer, Integer> expected = new HashMap<Integer, Integer>() {
      {
        put(0, 0);
        put(1, 1);
        put(2, 2);
      }
    };

    final Map<Integer, Integer> actual = Functions.id(Sets.Set(0, 1, 2));

    TestCase.assertEquals("class" + expected.getClass() + "/" + actual.getClass(), expected, actual);
  }

  @SuppressWarnings({ "serial", "unchecked" })
  public void testId_distinctResults() {
    Set<Map.Entry<Integer, Integer>> expected = new HashMap<Integer, Integer>() {
      {
        put(0, 0);
        put(1, 1);
        put(2, 2);
      }
    }.entrySet();

    Iterator<Map.Entry<Integer, Integer>> i = Functions.id(Sets.Set(0, 1, 2)).entrySet().iterator();
    final Map.Entry<Integer, Integer> e1 = i.next();
    final Map.Entry<Integer, Integer> e2 = i.next();
    final Map.Entry<Integer, Integer> e3 = i.next();
    Set<Map.Entry<Integer, Integer>> actual = Sets.Set(e1, e2, e3);
    assertTrue("extra?", expected.containsAll(actual));
    assertTrue("missing?", actual.containsAll(expected));
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testId_empty() {
    TestCase.assertTrue(Functions.id(Collections.EMPTY_SET).isEmpty());
  }

  <T> Functions.Function<T, String> functionNamed(final String name) {
    return new Functions.Function<T, String>() {
      @Override
      public String apply(T t) {
        return name + "(" + t + ")";
      }
    };
  }

  public void testMap_iterator() {
    Functions.Function<Integer, String> f = functionNamed("f");
    Iterator<String> iter = f.map(Arrays.asList(1, 2, 3).iterator());
    for (int i = 1; i < 4; i++) {
      assertEquals("f(" + i + ")", iter.next());
    }
    assertFalse(iter.hasNext());
  }

  public void testMap_iterable() {
    Functions.Function<Integer, String> f = functionNamed("f");
    Iterator<String> iter = f.map(((Iterable<Integer>) Arrays.asList(1, 2, 3))).iterator();
    for (int i = 1; i < 4; i++) {
      assertEquals("f(" + i + ")", iter.next());
    }
    assertFalse(iter.hasNext());
  }

  public void testMap_collection() {
    Functions.Function<Integer, String> f = functionNamed("f");
    Collection<String> c = f.map(((Collection<Integer>) Arrays.asList(1, 2, 3)));
    assertEquals(3, c.size());
    Iterator<String> iter = c.iterator();
    for (int i = 1; i < 4; i++) {
      assertEquals("f(" + i + ")", iter.next());
    }
    assertFalse(iter.hasNext());
  }

  public void testMap_list() {
    Functions.Function<Integer, String> f = functionNamed("f");
    List<String> list = f.map(Arrays.asList(1, 2, 3));
    assertEquals(3, list.size());
    assertEquals("f(3)", list.get(2));
  }

  public void testToMap() {
    Functions.Function<Integer, String> f = functionNamed("f");
    Map<Integer, String> actual = f.toMap(Sets.Set(1, 2, 3));
    assertEquals("f(1)", actual.get(1));
    assertEquals("f(2)", actual.get(2));
    assertEquals("f(3)", actual.get(3));
  }

  public void testForMap() {
    Functions.Function<Integer, String> f = functionNamed("f");
    Map<Integer, String> map = f.toMap(Sets.Set(1, 2, 3));
    Functions.Function<Integer, String> f1 = Functions.forMap(map);
    assertEquals("f(2)", f1.apply(2));
    assertEquals(null, f1.apply(4));
  }

  public void testCompose() {
    Functions.Function<Integer, String> f = functionNamed("f");
    Functions.Function<String, String> g = functionNamed("g");
    Functions.Function<Integer, String> fg = Functions.compose(f, g);
    assertEquals("g(f(42))", fg.apply(42));
  }

  public void testComposeInjections() {
    Functions.Injection<Integer, Integer> injection1 = new Functions.Injection<Integer, Integer>() {
      @Override
      public Integer apply(Integer i) {
        return i + 1;
      }
    };
    Functions.Injection<Integer, Integer> injection2 = new Functions.Injection<Integer, Integer>() {
      @Override
      public Integer apply(Integer i) {
        return i * 2;
      }
    };

    Functions.Function<Integer, Integer> composition = injection1.then(injection2);
    assertTrue(composition instanceof Functions.Injection);
  }
}